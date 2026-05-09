#!/usr/bin/bash
set -euo pipefail

# === Configuration ===
ROOT_DIR="${ROOT_DIR:-$(pwd)/riscv-compliance-ws}"
RISCV_PATH="/opt/riscv"
SRC_DIR="${ROOT_DIR}/src"
WORK_DIR="${ROOT_DIR}/work_act4"
OUT_DIR="${ROOT_DIR}/out/signatures/rv32i"
CONFIG_TEMPLATES_DIR="${ROOT_DIR}/config_templates"
DUT_CONFIG_DIR="$(pwd)/../esw/config/wardrv"

#JOBS="${JOBS:-$(nproc)}"
JOBS="${JOBS:-4}"
SAIL_VERSION="0.10"
ARCH_TEST_VERSION="${ARCH_TEST_VERSION:-4.0.0}"
RISCV_GCC_TOOLCHAIN_VERSION="${RISCV_GCC_TOOLCHAIN_VERSION:-2026.04.26}"

# URLs
GNU_TOOLCHAIN_URL="https://github.com/riscv/riscv-gnu-toolchain.git"
MISE_INSTALL_URL="https://mise.jdx.dev/install.sh"
SAIL_RELEASE_URL="https://github.com/riscv/sail-riscv/releases/download"
ARCH_TEST_REPO_URL="https://github.com/riscv-non-isa/riscv-arch-test.git"

# === Helpers ===
log_info()    { echo -e "\e[34m[INFO]\e[0m $1"; }
log_success() { echo -e "\e[32m[OK]\e[0m $1"; }
log_error()   { echo -e "\e[31m[ERROR]\e[0m $1" >&2; exit 1; }

show_help() {
    echo "Usage: $0 [options]"
    echo ""
    echo "Options:"
    echo "  -i, --init       Setup directories and install system dependencies (apt)"
    echo "  -g, --gcc        Build and install RISC-V GCC toolchain (v15+)"
    echo "  -m, --mise       Install mise tool manager"
    echo "  -s, --sail       Install Sail RISC-V reference model"
    echo "  -c, --compliance Setup architectural test suite and copy configs"
    echo "  -r, --run        Run compliance tests"
    echo "  -a, --all        Run all steps (full setup and test)"
    echo "  -h, --help       Show this help message"
}

# Check for sudo privileges and prompt if necessary.
# This avoids middle-of-script interruptions for password prompts.
# Arguments: None
# Returns:   None (exits on failure)
check_sudo() {
    if ! sudo -n true 2>/dev/null; then
        log_info "Sudo privileges required for system packages installation."
        sudo -v || log_error "Failed to obtain sudo privileges."
    fi
}

# Initialize the workspace directory structure.
# Arguments: None
# Globals:   RISCV_PATH, SRC_DIR, WORK_DIR, OUT_DIR
setup_directories() {
    check_sudo
    sudo mkdir -p "${RISCV_PATH}"
    mkdir -p "${SRC_DIR}" "${OUT_DIR}" "${WORK_DIR}" 
}

# Install OS-level dependencies via apt-get.
# Requires sudo privileges.
install_system_deps() {
    log_info "Installing required Ubuntu packages..."
    check_sudo
    sudo apt-get update -y || log_error "Failed to update apt packages."
    sudo apt-get install -y make curl git \
        device-tree-compiler libboost-all-dev autoconf automake  \
        autotools-dev python3 python3-pip python3-tomli gawk     \
        build-essential bison flex texinfo gperf libtool         \
        patchutils bc zlib1g-dev libexpat-dev ninja-build cmake  \
        libglib2.0-dev libslirp-dev libmpc-dev libmpfr-dev       \
        libgmp-dev pkg-config libncurses-dev
}

# Create symlinks for the 64-bit toolchain to be usable with 32-bit prefixes.
# Arguments: None
# Globals:   RISCV_PATH
setup_toolchain_links() {
    log_info "Configuring riscv64-unknown-elf aliases to point to 32-bit tools..."
    check_sudo
    for tool in gcc objdump objcopy; do
        local src_tool
        src_tool=$(command -v "riscv32-unknown-elf-${tool}") || { log_info "32-bit ${tool} not found, skipping link."; continue; }
        sudo ln -sf "${src_tool}" "${RISCV_PATH}/bin/riscv64-unknown-elf-${tool}"
        log_info "Aliased 32-bit ${tool} to 64-bit name."
    done
}

# Build and install the RISC-V GNU Toolchain (GCC 15+).
# This can take a significant amount of time.
# Globals: SRC_DIR, RISCV_PATH, JOBS, RISCV_GCC_TOOLCHAIN_VERSION
install_riscv_gcc_toolchain() {
    # Check if a sufficiently new GCC is already in the PATH
    if command -v riscv32-unknown-elf-gcc >/dev/null 2>&1; then
        local gcc_version_output=$(riscv32-unknown-elf-gcc --version 2>&1 | head -n 1)
        # Extract version number, e.g., "gcc (GNU) 15.0.0 20240420 (riscv-gnu-toolchain)" -> 15.0.0
        local gcc_version=$(echo "$gcc_version_output" | grep -oP '(?<=gcc \(GNU\) )\d+\.\d+\.\d+')
        
        if [ -n "$gcc_version" ] && (( $(echo "$gcc_version >= 15.0.0" | bc -l) )); then
            log_success "RISC-V 32-bit GCC toolchain (version ${gcc_version}) already present."
            return
        else
            log_info "Existing RISC-V 32-bit GCC (version ${gcc_version:-unknown}) is too old. Building a new one."
        fi
    fi

    log_info "Building RISC-V GNU Toolchain (this will take a long time)..."
    cd "${SRC_DIR}"
    [ -d riscv-gnu-toolchain ] || git clone -b "${RISCV_GCC_TOOLCHAIN_VERSION}" --recursive "${GNU_TOOLCHAIN_URL}" || log_error "Failed to clone riscv-gnu-toolchain."
    cd riscv-gnu-toolchain
    git checkout "${RISCV_GCC_TOOLCHAIN_VERSION}"
    git submodule update --init --recursive || log_error "Failed to initialize submodules for riscv-gnu-toolchain."

    local local_prefix="${SRC_DIR}/toolchain_install_tmp"
    log_info "Configuring RISC-V GNU Toolchain (Local build)..."
    mkdir -p "${local_prefix}"
    ./configure --prefix="${local_prefix}" --with-arch=rv32imac --with-abi=ilp32 || log_error "Failed to configure RISC-V GNU Toolchain."

    log_info "Building RISC-V GNU Toolchain (No Root)..."
    make -j"${JOBS}" || log_error "Failed to build RISC-V GNU Toolchain."

    log_info "Installing toolchain to ${RISCV_PATH} using Root..."
    check_sudo
    sudo cp -ra "${local_prefix}/." "${RISCV_PATH}/"
    rm -rf "${local_prefix}"
}

# Install 'mise' tool manager as recommended by the new ACT4 methodology.
install_mise() {
    if command -v mise >/dev/null 2>&1; then
        log_success "mise is already installed."
        return
    fi
    log_info "Installing mise tool manager..."
    curl "${MISE_INSTALL_URL}" | sh
}

# Install the Sail RISC-V Reference Model (v0.10).
install_sail() {
    if command -v sail_riscv_sim >/dev/null 2>&1; then
        log_success "Sail model already present."
        return
    fi
    log_info "Downloading Sail RISC-V Reference Model..."
    local arch=$(uname -m)
    local os=$(uname -s | tr '[:upper:]' '[:lower:]')
    check_sudo
    curl --location "${SAIL_RELEASE_URL}/${SAIL_VERSION}/sail-riscv-${os}-${arch}.tar.gz" \
        | sudo tar xvz --directory="${RISCV_PATH}" --strip-components=1
}

# Fetch the architectural test suite and prepare ACT4 configuration.
# Arguments: None
# Globals:   SRC_DIR, WORK_DIR, ARCH_TEST_VERSION
setup_compliance_suite() {
    log_info "Fetching riscv-arch-test suite..."
    cd "${SRC_DIR}"
    
    if [ ! -d riscv-arch-test/.git ]; then
        log_info "Cloning fresh architectural test suite..."
        rm -rf riscv-arch-test # Nettoie un éventuel dossier vide ou corrompu
        git clone -b "${ARCH_TEST_VERSION}" "${ARCH_TEST_REPO_URL}"
    else
        log_info "Suite already present, ensuring it's up to date..."
        (cd riscv-arch-test && git pull origin "${ARCH_TEST_VERSION}")
    fi
    
    # Trust the mise configuration in the repository
    (cd riscv-arch-test && "${HOME}/.local/bin/mise" trust .mise.toml)

}

# Execute tests using the ACT4 Framework (Makefile).
# Arguments: None
# Globals:   WORK_DIR, SRC_DIR, OUT_DIR
run_compliance() {
    log_info "Running ACT4 Framework tests..."
    cd "${SRC_DIR}/riscv-arch-test"
    
    # 1. Generate the assembly tests and coverpoints
    log_info "Generating assembly tests..."
    make tests --jobs "${JOBS}"

    # 1.5. Run with the default configuration first (CVW)
    log_info "Running default configuration (CVW)..."
    CONFIG_FILES="config/cores/cvw/cvw-rv64gc/test_config.yaml" \
    WORKDIR="${WORK_DIR}/cvw_default" \
    EXTENSIONS="I,Zicsr" \
    make --jobs "${JOBS}"

    ## 2. Build the ELFs for our DUT (asylum_wardrv)
    ## ACT4 uses make with CONFIG_FILES variable
    log_info "Running custom DUT configuration..."
    CONFIG_FILES="${DUT_CONFIG_DIR}/test_config.yaml" \
    WORKDIR="${WORK_DIR}" \
    EXTENSIONS="I,Zicsr" \
    make --jobs "${JOBS}"

    log_info "Tests completed. ELFs available in ${WORK_DIR}/asylum_wardrv/elfs"
}

# === Main Execution ===

# Entry point orchestrating the full setup and execution flow.
# Arguments: $@ (Passed from script arguments)
# Returns:   None
main() {
    if [[ $# -eq 0 ]]; then
        show_help
        exit 0
    fi

    local run_init=false
    local run_gcc=false
    local run_mise=false
    local run_sail=false
    local run_comp=false
    local run_run=false

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -i|--init)       run_init=true; shift ;;
            -g|--gcc)        run_gcc=true; shift ;;
            -m|--mise)       run_mise=true; shift ;;
            -s|--sail)       run_sail=true; shift ;;
            -c|--compliance) run_comp=true; shift ;;
            -r|--run)        run_run=true; shift ;;
            -a|--all)
                run_init=true; run_gcc=true; run_mise=true;
                run_sail=true; run_comp=true; run_run=true;
                shift ;;
            -h|--help)       show_help; exit 0 ;;
            *) log_error "Unknown option: $1"; show_help; exit 1 ;;
        esac
    done

    log_info "Starting RISC-V Compliance Workspace tasks"
    
    export PATH="${RISCV_PATH}/bin:${HOME}/.local/bin:${PATH}"

    if [[ "$run_init" == true ]]; then
        setup_directories
        install_system_deps
    fi
    if [[ "$run_gcc" == true ]]; then
        install_riscv_gcc_toolchain
        setup_toolchain_links
    fi
    [[ "$run_mise" == true ]] && install_mise
    [[ "$run_sail" == true ]] && install_sail
    [[ "$run_comp" == true ]] && setup_compliance_suite
    [[ "$run_run"  == true ]] && run_compliance

    echo -e "\n======================================="
    log_success "Workspace Ready."
    echo " - GCC:        $(command -v riscv32-unknown-elf-gcc) (aliased to riscv64)"
    echo " - Workdir:    ${WORK_DIR}"
    echo " - Signatures: ${OUT_DIR}"
    echo "======================================="
}

main "$@"
