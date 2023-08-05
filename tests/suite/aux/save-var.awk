BEGIN {
    par_num = 0;
    par_txt = "";
    file_name = "suite/toml/lorem-ipsum-100-vars.toml"
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
        printf ("par%02d = '''%s'''\n\n", par_num, par_txt) >> file_name;
        par_num += 1;
        par_txt = "";
    }
}

END {
    printf ("par%02d = '''%s'''\n", par_num, par_txt) >> file_name;
}
