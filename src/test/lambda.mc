int lambda(func f, int a) {
    func b = int c () {
        return f(a);
    };
    return b();
}

int main() {
    int k = 1; 
    func a = int b() {
        return k;
    };
    return lambda(a, 1);
}
