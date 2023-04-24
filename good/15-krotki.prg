int main() {
    Tuple(int, string) t1 = (6, "str");
    Tuple(bool) t2 = newTuple (false);
    
    print t1;
    print t2;

    (x, y) = t1;
    print x;
    print y;

    x = 10;
    print t1;
    print x;
}