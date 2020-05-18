
int main() {
    List a;
    a.add(1);
    a.add(2);
    a.add(3);
    func f = int b(int x) {
        return x + 10;
    };
    print(a);
    List b = a.map(f);
    print(b);
}
