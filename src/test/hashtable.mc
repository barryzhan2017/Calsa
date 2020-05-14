int main() {
    Hashtable a;
    a.set(1, 2);
    a.set(321, 5);
    print(a.get(0));
    print(a.get(1));
    print(a.get(321));
    a.set(1, 5);
    print(a.get(1));

    print(a);
}
