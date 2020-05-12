int main() {
    List a;
    int i = 0;
    int k = 0;

    while(i < 10){
        a.add(i);
        a.print();
        i = i + 1;
    }

    while (k < 9){
        a.remove(0);
        a.print();
        k = k + 1;
    }
}
