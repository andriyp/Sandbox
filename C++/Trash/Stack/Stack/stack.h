#ifndef STACK_H
#define STACK_H

#include <vector>
#include <list>

using namespace std;

template <template <class,class> class S, class T, class A = allocator<T>>
struct Stack {
    static void push  (S<T,A>&, T);
    static T    pop   (S<T,A>&);
    static bool empty (S<T,A>&);
};

template <class T, class A>
struct Stack<vector,T,A> {    
    static void push (vector<T,A>& vec, T elem) {
        vec.push_back(elem);
    }
    
    static T pop (vector<T,A>& vec) {
        auto x = vec.back();
        vec.pop_back();
        return x;
    }

    static bool empty (vector<T,A>& vec) {
        return vec.empty();
    }
};

#endif