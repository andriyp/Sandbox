#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <array>
#include <set>

namespace S = std;

namespace p1_arrays_and_strings {
    // whether every character occurence in string is only occurence of that character
    bool task_1_1 (S::string str) {
        typedef decltype(str.length()) T;

        for (T i = 0; i < str.length() - 1; ++i)
            for (T j = i + 1; j < str.length(); ++j)
                if (str[i] == str[j])
                    return false;

        return true;
    }

    // whether one string is permutation of another
    bool task_1_3 (S::string strA, S::string strB) {
        S::sort(strA.begin(), strA.end());
        S::sort(strB.begin(), strB.end());
        return strA == strB;
    }

    // replace all spaces in string by %20
    void task_1_4 (S::string& str) {
        for (char& c : str)
            c = (c == ' ') ? '%20' : c;
    }

    // packed string
    S::string task_1_5 (const S::string& str) {
        typedef decltype(str.length()) T;
    
        T len = str.length();

        if (len < 2) return str;

        S::vector<char> chars;
    
        char c = str[0];
        int i = 1, k = 1;

        for (;;) {
            while (i < len && str[i] == c)
                ++i, ++k;

            chars.push_back(c);
            chars.push_back('0'+k);

            c = str[i];
            k = 0;

            if (i == len) break;
        }
    
        return (chars.size() > len)
            ? str
            : S::string(chars.begin(), chars.end());
    }

    // rotate matrix 90 degrees
    void task_1_6 (int arr[][3], size_t sz) {
        for (int i = 0; i < sz; ++i) {
            for (int j = i + 1; j < sz; ++j)
                S::swap(arr[i][j], arr[j][i]);

            int l = sz / 2, r = l;
            if (sz % 2 == 0) ++r;

            while (l >= 0)
                S::swap(arr[i][l--], arr[i][r++]);
        }
    }

    // if arr[m][n] == 0, then set to 0 whole row and column
    template <size_t M, size_t N> void task_1_7 (int (&arr)[M][N], int m, int n) {
        if (arr[m][n] != 0) return;

        for (int i = 0; i < N; ++i) arr[m][i] = 0;
        for (int j = 0; j < M; ++j) arr[j][n] = 0;
    }

    bool is_substring (S::string strA, S::string strB) {
        return strB.find(strA) != S::string::npos;
    }

    // test whether one string is cycle shifted variant of another, using only one call to isSubstring
    bool task_1_8 (S::string strA, S::string strB) {
        return strA.length() == strB.length()
            && is_substring(strA, strB+strB);
    }
}

namespace laba2_linked_lists
{
    template <class T> struct list_node {
        T elem;
        list_node<T> *next;
        
        list_node (T elem, list_node<T> *next)
            : elem (elem)
            , next (next)
        {}        
    };

    template <class T> class list {    
    public:
        typedef list_node<T> node_type;                        

        list ()
            : node (NULL)
        {}

        ~list ();
        list (const list&);
        
        void    insert            (T);
        T       nth               (int);
        list<T> reverse           ();
        void    remove_duplicates ();
        void    print             ();        

    private:
        node_type *node;
    };

    template <class T>
    list<T>::~list ()
    {
        node_type *next, *cur = node;

        while (cur != NULL)
        {
            next = cur->next;
            delete cur;
            cur = next;
        }
    }

    template <class T>
    list<T>::list (const list& from)
    {
        if (from.node == NULL) return;

        node = new node_type(from.node->elem, NULL);

        node_type *prev = node;

        for (node_type *cur = from.node->next; cur != NULL; cur = cur->next)
        {
            prev->next = new node_type(cur->elem, NULL);
            prev = prev->next;
        }
    }

    template <class T>
    void list<T>::insert (T elem)
    {
        node = new node_type(elem, node);
    }

    template <class T>
    T list<T>::nth (int index)
    {
        node_type *cur;

        for (cur = node; index > 0; --index)
            cur = cur->next;

        return cur->elem;
    }

    template <class T>
    list<T> list<T>::reverse ()
    {
        list<T> result;
                
        for (node_type *cur = node; cur != NULL; cur = cur->next)        
            result.insert(cur->elem);
        
        return result;
    }

    template <class T>
    void list<T>::print ()
    {
        for (node_type *cur = node; cur != NULL; cur = cur->next)
            std::cout << cur->elem << ' ';
    }

    template <class T>
    void list<T>::remove_duplicates ()
    {
        if (node == NULL) return;

        std::set<T> entries;        
        node_type   *prev = node;

        for (node_type *cur = node; cur->next != NULL; cur = cur->next)
        {
            if (entries.find(cur->elem) != entries.end())
            {
                node_type *hanging_node = cur->next;

                cur->elem = cur->next->elem;                
                cur->next = cur->next->next;                

                delete hanging_node;

                if (cur->next == NULL)
                    break;

                cur = prev;
            }
            else            
                entries.insert(cur->elem);            

            prev = cur;
        }

        if (prev->next != NULL && entries.find(prev->next->elem) != entries.end())
        {
            node_type *hanging_node = prev->next;
            prev->next = NULL;
            delete hanging_node;
        }
    }
}

int main (int argc, char **argv)
{
    using namespace laba2_linked_lists;
    
    list<int> lst;
    
    lst.insert(1);
    lst.insert(2);    
    lst.insert(3);
    lst.insert(1);
    lst.insert(2);
    lst.insert(2);
    lst.insert(3);
    
    lst.remove_duplicates();

    // auto lst2 = lst;

    lst.reverse().print();

    std::getchar();
}