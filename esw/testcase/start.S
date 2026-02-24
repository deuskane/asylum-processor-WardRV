.section .text
.global _start

_start:
    # Initialisation du pointeur de pile (Stack Pointer)
    # On le place à la fin de la RAM
    li sp, 0x80010000

    # Appel de la fonction main
    call main

    # Boucle infinie si main retourne (pour éviter de partir dans le décor)
loop:
    j loop
