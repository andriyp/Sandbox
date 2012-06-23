#include <iostream>
#include "stack.h"

using namespace std;

template <template <class,class> class C, class T, class A>
void foo(C<T,A>& xs, T x) {
    typedef Stack<C,T,A> S;
    S::push(xs, x + 1);
    S::push(xs, x + 2);
    S::push(xs, x + 3);

    while(!S::empty(xs))
        cout << S::pop(xs);
}

int main () {
    int xs[] = { 2, 3 };
    auto vec = vector<int>(xs, xs+2);
    
    foo(vec, 0);

    system("pause");
}