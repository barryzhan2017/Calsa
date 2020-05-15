int main() {
    Hashtable a;
    int i = 0;
    while (i < 50) {
        i = i + 1;
        a.set(i, 100-2*i);
    }
    print(a);
    print(a.get(25));
}
