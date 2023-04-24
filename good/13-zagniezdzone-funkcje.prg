int x = 1;
void f() {
    print x;
}

void g() {
    print x;
    x = 10;

    void f() {
        print x;
    }

    f();
    x = 20;
    f(); 
}

int main() {
    f();
    g();
}