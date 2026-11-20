#===================================================================
# Variables
#===================================================================
# Configuration
DRAWIO_APP         ?= drawio
SCALE               = 2
FORMAT              = png

SOURCES             = $(wildcard *.drawio)

# Timing diagrams : WaveDrom source <name>.json, rendered <name>.svg
WAVEDROM_APP       ?= wavedrompy
WAVE_SOURCES        = $(wildcard *.json)
WAVE_TARGETS        = $(WAVE_SOURCES:.json=.svg)

RULE_IMAGE_TARGETS := $(shell \
	for f in $(SOURCES); do \
		base=$$(basename $$f .drawio); \
		grep -o 'name="[^"]*"' "$$f" | sed 's/name="//;s/"//' | \
		( \
		idx=1; \
		while read name; do \
			echo "$${f}#$${idx}#$${base}-$${name}.$(FORMAT)"; \
			idx=$$(($${idx}+1)); \
		done; \
		); \
		rm -f "$$f.xml"; \
	done \
)

#===================================================================
# Target
#===================================================================
.DEFAULT_GOAL       = all

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
define GEN_RULE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1 : source file
# 2 : dest file
# 3 : page indexe

IMAGE_TARGETS += $(2)

#-------------------------------------------------------------------
$(2): $(1)
#-------------------------------------------------------------------
	$(DRAWIO_APP) --export --format $(FORMAT) --scale $(SCALE) --page-index $(3) --crop --output "$(2)" "$(1)"
endef

$(foreach tgt,$(RULE_IMAGE_TARGETS),$(eval $(call GEN_RULE,$(shell echo "$(tgt)" | cut -d'#' -f1),$(shell echo "$(tgt)" | cut -d'#' -f3),$(shell echo "$(tgt)" | cut -d'#' -f2))))

#-------------------------------------------------------------------
# Timing diagram of a WaveDrom source
%.svg: %.json
#-------------------------------------------------------------------
	$(WAVEDROM_APP) -i "$<" -s "$@"
	@sed -i '0,/<svg[^>]*>/s##&<rect width="100%" height="100%" fill="white"/>#' "$@"

#-------------------------------------------------------------------
all: $(IMAGE_TARGETS) $(WAVE_TARGETS)
#-------------------------------------------------------------------
	@echo "--- Toutes les images sont à jour ---"

.PHONY: all

#-------------------------------------------------------------------
clean:
#-------------------------------------------------------------------
	rm -f $(IMAGE_TARGETS) $(WAVE_TARGETS)

.PHONY: clean
