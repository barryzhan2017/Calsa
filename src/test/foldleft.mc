int main() {
    List list;

    int i = 1;
    while (i < 11) {
        list.add(i);
        i = i + 1;
    }

    func f = int sum(int x, int y) {
        return x + y;
    };

    print("The original list is");
    print(list);
    print("The reduced result is");
    print(list.foldleft(0, f));
}
