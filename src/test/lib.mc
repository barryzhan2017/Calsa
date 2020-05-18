int map(List l, func f){
    int len = l.size();
    int i = 0;
    while (i < len){
        int tmp = f(l.get(i));
        l.set(i, tmp);
        i = i + 1;
    }
    return 1;
}