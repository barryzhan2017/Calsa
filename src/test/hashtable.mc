int main() {
    Hashtable a;
    int i = 0;
    while (i < 50) {
        i = i + 1;
        a.set(i, 100-2*i);
    }
    print(a);
    print(a.get(25));
    print(a.hasKey(31231));
    print(a.hasKey(3));
    a.remove(2);
    print(a);

    Hashtable b = a;
    print(b);
}
