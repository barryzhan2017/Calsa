int main() {
    Hashset a;
    Hashset b;
    Hashset c;
    int i = 0;
    while (i < 50) {
        i = i + 1;
        a.add(i);
        b.add(i);
    }
    print(a);
    a.exist(25);
    a.remove(25);
    print(a);
}
