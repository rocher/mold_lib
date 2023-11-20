BEGIN {
    par_num = 0;
    par_txt = "";
    fnm = "suite/molt/lorem-ipsum-%02d.molt"
}

{
    if (length($0) > 0) {
        if (par_txt == "") {
            par_txt = $0;
        }
        else {
            par_txt = par_txt "\n" $0
        }
    }
    else {
        filename = sprintf (fnm, par_num);
        printf ("%s\n", par_txt) >> filename;
        par_num += 1;
        par_txt = "";
    }
}

END {
    filename = sprintf (fnm, par_num);
    printf ("%s\n", par_txt) >> filename;
}
