int main() {
    List b;
    int i = 0;
    while (i < 10) {
        b.add(i);
        i = i + 1;
    }
    func l = int b(int x, int y) {
        return x + y;
    };
    print(b);
    print(b.foldleft(0, l));
}
