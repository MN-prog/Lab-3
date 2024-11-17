#include <iostream>

using namespace std;

void sl(long long arr[], long long l, long long mid, long long r) {
    long long n1 = mid - l + 1; 
    long long n2 = r - mid;    

    long long* L = new long long[n1];
    long long* R = new long long[n2];

    for (long long i = 0; i < n1; i++)
        L[i] = arr[l + i];
    for (long long j = 0; j < n2; j++)
        R[j] = arr[mid + 1 + j];

    long long i = 0;
    long long j = 0;
    long long k = l;

    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i++;
        } else {
            arr[k] = R[j];
            j++;
        }
        k++;
    } 

    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }

    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }

    delete[] L;
    delete[] R;
}

void slSort(long long arr[], long long l, long long r) {
    if (l < r) {
        long long mid = l + (r - l) / 2;

        slSort(arr, l, mid);
        slSort(arr, mid + 1, r);

        sl(arr, l, mid, r);
    }
}
void arrsum(long long int arr[], long long int size, long long int weights_array[])
{
    for(long long int mask = 0; mask < (1 << size); ++mask)
    {
        long long sum = 0;
        for(long long int i = 0; i < size; ++i)
        {   
            if (mask & (1 << i)) {
                sum += weights_array[i];
            }
        }
        arr[mask] = sum;
    }

}

long long bnSearch(long long int arr[], long long int size, long long int gt) {
    int l = 0, r = size - 1;
    long long ans = -1;

    while (l <= r) {
        int mid = l + (r - l) / 2;

        if (arr[mid] <= gt) {
            ans = arr[mid];  // Запоминаем текущий лучший результат
            l = mid + 1;     // Ищем дальше справа
        } else {
            r = mid - 1;    // Ищем в левой части
        }
    }

    return ans;  // Возвращаем непосредственно найденное значение
}

int main() {
    int n;
    cin >> n;

    // Разделение массива на две части: левую и правую
    long long int l[n / 2] = {}, r[n / 2 + n % 2] = {};
    long long total_sum = 0;

    // Ввод первой половины массива и суммирование
    for (int i = 0; i < n / 2; i++) {
        cin >> l[i];
        total_sum += l[i];
    }

    // Ввод второй половины массива и суммирование
    for (int i = 0; i < (n / 2 + n % 2); i++) {
        cin >> r[i];
        total_sum += r[i];
    }

    // Массивы для хранения всех возможных сумм подмножеств
    long long int lSum[1 << n / 2], rsum[1 << (n / 2 + n % 2)];

    // Заполнение массивов с суммами подмножеств для левой и правой частей
    arrsum(lSum, n / 2, l);
    arrsum(rsum, n / 2 + n % 2, r);

    // Сортировка всех возможных сумм для правой части
    slSort(rsum, 0, (1 << (n / 2 + n % 2)) - 1);

    // Инициализация переменной для минимальной разницы
    long long min_diff = total_sum;
    long long gt = total_sum / 2;

    // Перебор всех возможных сумм левой части
    for (long long left_sum : lSum) {
        if (left_sum <= gt) {
            // Ищем ближайшую сумму в правых подмножествах с помощью бинарного поиска
            long long closest_sum = bnSearch(rsum, 1 << (n / 2 + n % 2), gt - left_sum);
            // Рассчитываем минимальную разницу
            min_diff = min(min_diff, abs(total_sum - 2 * (left_sum + closest_sum)));
        }
    }

    cout << min_diff << endl;

    return 0;
}


