#include "iostream";
void toto();

void toto() {
    std::cout << "Hello!\n";
}
void titi() {
    toto();
}

class Foo {
public:
    int toto_;
    void FooBar() {
        std::cout << "Hi\n" << 6;
    }
};

int main(int argc, char const *argv[])
{
    toto();
    Foo foo = Foo();
    foo.FooBar();
    foo.FooBar();
    foo.FooBar();
    return 0;
}