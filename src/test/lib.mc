List map(List l, func f){
    int len = l.size();
    int i = 0;
    while (i < len){
        int tmp = f(l.get(i));
        l.set(i, tmp);
        i = i + 1;
    }
    return l;
}

int foldleft(List l, int acc, func f) {
    int len = l.size();
    int i = 0;
    while (i < len){
        acc = f(acc, l.get(i));
        i = i + 1;
    }
    return acc;
}

List filter(List l, func f) {
    int i = l.size();
    while (0 < i){
        int temp = f(l.get(i-1));
        if (temp < 1) {
            l.remove(i-1);
        }
        else {}
        i = i - 1;
    }
    return l;
}