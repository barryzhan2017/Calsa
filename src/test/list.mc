
int main() {
    List a;
    a.add(1);
    a.add(2);
    a.add(3);
    func f = int b(int x) {
        return x + 10;
    };
    a.print();
    map(a, f);
    a.print();
}
