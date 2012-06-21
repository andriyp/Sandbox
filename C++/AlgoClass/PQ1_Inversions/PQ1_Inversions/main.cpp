#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

long long inplaceInvCountingMergeSort (vector<int>& vec, int start = 0, int end = -1) {
    if (end == -1)
        end = vec.size() - 1;

    if (start == end)
        return 0;

    if (start + 1 == end) {
        if (vec[start] > vec[end]) {
            swap(vec[start], vec[end]);
            return 1;
        }
        return 0;
    }
    
    int len = end - start + 1,
        mid = (start + end + 1) / 2;

    long long
        l = inplaceInvCountingMergeSort(vec, start, mid-1),
        c = 0,
        r = inplaceInvCountingMergeSort(vec, mid, end);    

    auto tmpv = vector<int>(len);

    for (int i = start, j = mid, k = 0; k < len; ++k) {
        if (i < mid && (j > end || vec[i] < vec[j])) {
            tmpv[k] = vec[i++];
        } else {
            tmpv[k] = vec[j++];
            c += mid - i;
        }
    }

    for (int k = 0; k < len; ++k)
        vec[start + k] = tmpv[k];

    return l + c + r;
}

int main (int argc, char **argv) {    
    ifstream ifs ("S:\\prog\\sandbox\\C++\\AlgoClass\\PQ1_Inversions\\IntegerArray.txt");
    auto xs = vector<int> ();    
    
    int x;
    while (ifs >> x)
        xs.push_back(x);
    
    cout << inplaceInvCountingMergeSort(xs);
    cin.get();
}

/* gave up on this ):
void inplaceMerge (vector<int>& vec) {
    int mid = vec.size() / 2;
    bool tmpSet = false;
    for (int tmp, i = 0, j = mid; i != j;) {
        if (vec[i] > vec[j]) {
            if (!tmpSet) {
                tmp = vec[j++];
                tmpSet = true;
            } else {
                for (int k = j-1; k > i; --k)
                    vec[k] = vec[k-1];
                // vec[i+1] = vec[i];
                vec[i++] = tmp;
                tmp = vec[j++];
            }
        } else {
            if (tmpSet)
                swap(tmp, vec[i]);
            ++i;
        }
    }
} */
