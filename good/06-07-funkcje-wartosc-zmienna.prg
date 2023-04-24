int fib(int n) {
    if (n <= 1) {
        return n;
    }
    else {
        int a =  fib(n - 1);
        int b = fib(n - 2);
        return a + b;
    } 
}

void referenceFib(int n, int &result) {
    result = fib(n);
}

int main() {
    print "Fib(5) = " + (toString fib(5));

    int a = 3;
    print "Fib(3) = " + (toString fib(a));

    referenceFib(6, a);
    print "Fib(6) = " + (toString a);
}