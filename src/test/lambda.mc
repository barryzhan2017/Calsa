int lambda(func f, int a) {
    func b = int c () {
        return f(a);
    };
    return b();
}

int main() {
    int k = 1; 
    func a = int b(int x) {
         return k + 10;
     };
     print(lambda(a, 100));
     return 0;
}
