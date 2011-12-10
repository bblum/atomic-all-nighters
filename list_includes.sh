for i in `ls -d 410kern/*/`; do echo -n "-I $i "; done
echo -n "-I 410kern/ "
echo -n "-I kern/ "
echo -n "-I kern/inc/ "
echo -n "-I spec/ "
