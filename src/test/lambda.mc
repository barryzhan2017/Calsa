int lambda(func f, int a) {
    func b = int c () {
        return f(a);
    };
    return b();
}

int main() {
    int k = 1; 
    print(lambda(1));
}
