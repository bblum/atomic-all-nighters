/**
 * The 15-410 kernel project.
 * @name 410loader.c
 *
 * Functions for the loading
 * of user programs from binary
 * files should be written in
 * this file. The function
 * elf_load_helper() is provided
 * for your use.
 */
/*@{*/

/* --- Includes --- */
#include <elf/elf_410.h>
#include <exec2obj.h>
     /** FIXME: Should probably use something other than lprintf? */
#include <simics.h>
#include <stdio/stdio.h>
#include <string/string.h>
#include <malloc.h>


/**
 * Checks to see if file with name fname is an elf
 * executable binary. If so, fills in se_hdr struct
 * and returns ELF_SUCCESS. If not, returns ELF_NOTELF.
 * Assumes that the magic numbers in the header have been
 * verified.
 *
 * @param se_hdr   pointer to simple_elf_t struct to be
 *                 filled in. Memory for it has been
 *                 allocated.
 *
 * @param fname    name of file to use.
 *
 * @return   ELF_SUCCESS if se_hdr was successfully filled in.
 *           ELF_NOTELF if the file is not the kind of binary
 *           we are using in 15-410.
 */
int
elf_load_helper( simple_elf_t *se_hdr, const char *fname )
{
    Elf32_Ehdr elf_hdr;           /* elf header */
    Elf32_Shdr *elf_sec_hdrs;     /* section headers */
    char *shstrtab;               /* sectcion header string table */
    unsigned int ret;             /* various return values */
    unsigned int size;            /* for holding size of various things */
    unsigned int i;               /* loop index */

    memset(se_hdr, 0, sizeof(simple_elf_t));

    /* 
     * Grab the elf header 
     */
    ret = getbytes(fname, 0, sizeof( Elf32_Ehdr ), (char *)&elf_hdr);
    if( ret != sizeof( Elf32_Ehdr ) ) {
        lprintf( "Loader: Couldn't read elf header: %d, %s", ret, fname );
        return ELF_NOTELF;
    }

    /*
     * grab the entry point and file name.
     */
    se_hdr->e_entry = elf_hdr.e_entry;
    se_hdr->e_fname = fname;

    /*
     * Sections that are probably in a simple ELF binary are 
     * with debugging symbols are:
     *  .symtab
     *  .strtab  
     *  .shstrtab
     *  .text  
     *  .rodata
     *  .data
     *  .bss 
     *  .stab   
     *  .stabstr
     *  .comment
     *  .note
     */
    /* This method of checking has been replaced with a more intelligent
       method which actually attempts to recognize all of the sections

    if( elf_hdr.e_shnum > 12 ) {
        lprintf( "Loader: too many section headers" );
    } 
    */

    size = elf_hdr.e_shnum * sizeof( Elf32_Shdr );

    /*
     * Allocate memory for table;
     */
    elf_sec_hdrs = (Elf32_Shdr *)malloc( size );
    if( !elf_sec_hdrs ) {
        lprintf( "Loader: not enough mem for sec_hdr array" );
        return ELF_NOTELF;
    }

    /*
     * Grab array of section headers
     */
    ret = getbytes( fname, elf_hdr.e_shoff, size, (char *)elf_sec_hdrs );
    if( ret != size ) {
        lprintf( "Loader: could not read section headers" );
        free( elf_sec_hdrs );
        return ELF_NOTELF;
    }

    /*
     * I want the section header string table first
     * so I know what other entries are.
     */
    size = elf_sec_hdrs[elf_hdr.e_shstrndx].sh_size;
    shstrtab = (char *)malloc( size );
    if( !shstrtab ) {
        lprintf( "Loader: not enought mem for sec hdr str tab" );
        free( elf_sec_hdrs );
        return ELF_NOTELF;
    }

    ret = getbytes( fname, elf_sec_hdrs[elf_hdr.e_shstrndx].sh_offset,
                    size, shstrtab );
    if( ret != size ) {
        lprintf( "Loader: could not read sec hdr str tab" );
        free( shstrtab ); free( elf_sec_hdrs );
        return ELF_NOTELF;
    }

    /*
     * loop to find .text .rodata .data .bss section headers.
     */
    for( i = 0; i < elf_hdr.e_shnum; i++ ) {
        unsigned int str_idx;

        if( elf_sec_hdrs[i].sh_name == SHN_UNDEF ) {
            continue;
        }

        str_idx = elf_sec_hdrs[i].sh_name;
        if( strcmp( ".text", &(shstrtab[ str_idx ]) ) == 0 ) {
            /* 
             * This section header is for the text segment 
             */

            se_hdr->e_txtoff   = elf_sec_hdrs[i].sh_offset;
            se_hdr->e_txtlen   = elf_sec_hdrs[i].sh_size;
            se_hdr->e_txtstart = elf_sec_hdrs[i].sh_addr;
        }
        else if( strcmp( ".rodata", &(shstrtab[ str_idx ]) ) == 0 ) {
            /*
             * This section header is for the rodata segment
             */

            se_hdr->e_rodatoff   = elf_sec_hdrs[i].sh_offset;
            se_hdr->e_rodatlen   = elf_sec_hdrs[i].sh_size;
            se_hdr->e_rodatstart = elf_sec_hdrs[i].sh_addr;
        }
        else if( strcmp( ".data", &(shstrtab[ str_idx ]) ) == 0 ) {
            /*
             * This section header is for the data segment
             */

            se_hdr->e_datoff   = elf_sec_hdrs[i].sh_offset;
            se_hdr->e_datlen   = elf_sec_hdrs[i].sh_size;
            se_hdr->e_datstart = elf_sec_hdrs[i].sh_addr;
        }
        else if( strcmp( ".bss", &(shstrtab[ str_idx ]) ) == 0 ) {
            /*
             * This section header is for the bss segment
             */

            se_hdr->e_bsslen = elf_sec_hdrs[i].sh_size;
            se_hdr->e_bssstart = elf_sec_hdrs[i].sh_addr;
        }
	else if (strcmp( ".symtab", &(shstrtab[ str_idx ])) != 0 &&
		 strcmp( ".strtab", &(shstrtab[ str_idx ])) != 0 &&
		 strcmp( ".shstrtab", &(shstrtab[ str_idx ])) != 0 &&
		 strcmp( ".stab", &(shstrtab[ str_idx ])) != 0 &&
		 strcmp( ".stabstr", &(shstrtab[ str_idx ])) != 0 &&
		 strcmp( ".comment", &(shstrtab[ str_idx ])) != 0 &&
		 strcmp( ".note", &(shstrtab[ str_idx ])) != 0 &&
		 strncmp( ".debug", &(shstrtab[ str_idx ]), 6) != 0) {
	  lprintf( "Loader: unknown header: \"%s\"",
			&(shstrtab[ str_idx ]));
	}
		
    }

    free( shstrtab ); free( elf_sec_hdrs );
    return ELF_SUCCESS;
}

/**
 * Checks fields in the potential ELF header to
 * make sure that is actually and ELF header.
 *
 * @param fname  file name of potential ELF binary
 *
 * @return  ELF_SUCCESS if it is an ELF header.
 *          ELF_NOTELF if it isn't.
 */
int
elf_check_header( const char *fname )
{
    Elf32_Ehdr elf_hdr;  
    unsigned int ret;

    /*
     * read the elf header from the file.
     */
    ret = getbytes(fname, 0, sizeof( Elf32_Ehdr ), (char *)&elf_hdr);
    if( ret != sizeof( Elf32_Ehdr ) ) {
        lprintf( "Loader: Couldn't read elf header: %d, %s", ret, fname );
        return ELF_NOTELF;
    }

    if( elf_hdr.e_ident[0] != 0x7f ||
        elf_hdr.e_ident[1] != 'E'  ||
        elf_hdr.e_ident[2] != 'L'  ||
        elf_hdr.e_ident[3] != 'F'  ) {

        return ELF_NOTELF;
    }

    if( elf_hdr.e_type != ET_EXEC ) {
        return ELF_NOTELF;
    }

    if( elf_hdr.e_machine != EM_386 ) {
        return ELF_NOTELF;
    }

    if( elf_hdr.e_version != EV_CURRENT ) {
        return ELF_NOTELF;
    }

    return ELF_SUCCESS;
}

/*@}*/
