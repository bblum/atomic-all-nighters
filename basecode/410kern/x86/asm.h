/** @file x86/asm.h
 *  @brief x86-specific assembly functions
 *  @author matthewj S2008
 */

#ifndef X86_ASM_H
#define X86_ASM_H

#include <aan.h>
#include <stdint.h>

/** @brief Loads a new gdt */
WONT_SLEEP void lgdt(void *gdt, unsigned int limit);
/** @brief Loads a new idt */
WONT_SLEEP void lidt(void *idt, unsigned int limit);
/** @brief Read address of IDT */
WONT_SLEEP void *idt_base(void);

/** @brief Disables interrupts */
ENTER_ATOMIC void disable_interrupts();
/** @brief Enables interrupts */
EXIT_ATOMIC void enable_interrupts();

/** @brief Read from the TSC */
WONT_SLEEP uint64_t rdtsc();

/** @brief Reads 1 byte from given port */
WONT_SLEEP uint8_t inb(uint16_t port);
/** @brief Reads 2 bytes from given port */
WONT_SLEEP uint16_t inw(uint16_t port);
/** @brief Reads 4 bytes from given port */
WONT_SLEEP uint32_t ind(uint16_t port);

/** @brief Writes 1 byte to target port */
WONT_SLEEP void outb(uint16_t port, uint8_t val);
/** @brief Writes 2 bytes to target port */
WONT_SLEEP void outw(uint16_t port, uint16_t val);
/** @brief Writes 4 bytes to target port */
WONT_SLEEP void outd(uint16_t port, uint32_t val);

/** @brief Delay 1/8 microsecond */
MAY_SLEEP void iodelay(void);

#endif /* !X86_ASM_H */
