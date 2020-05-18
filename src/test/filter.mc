int main() {
    List a;
    a.add(1);
    a.add(20);
    a.add(30);
    a.add(4);
    func f = int b(int x) {
        if (x < 10) {
            return 1;
        }
        else {
            return 0;
        }
    };
    print(a);
    a.filter(f);
    print(a);
}
