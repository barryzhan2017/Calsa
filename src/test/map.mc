
int main() {
    List list;

    int i = 1;
    while (i < 11) {
        list.add(i);
        i = i + 1;
    }

    func f = int too_odd (int x) {
        return 2 * x - 1;
    };

    print("The original list is");
    print(list);
    List mapped_list = list.map(f);
    print("The mapped list is");
    print(mapped_list);
}
