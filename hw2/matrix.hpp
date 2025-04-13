#pragma once
#include <iostream>
#include <fstream>
#include <cmath>
#include <string>
#include <math.h>

using namespace std;

class  matrix
{
    public:
    int line;
    int column;
    double** data;

    void swap(int line1, int line2){

    if (line1 >= 0 && line1 <= line && line2 >= 0 && line2 <= line){
        for (int i = 0; i < column; ++i){
            double temp = data[line1][i];
            data[line1][i] = data[line2][i];
            data[line2][i] = temp;
        }
    }
}

void multiplication(int a, double temp){
    if (a >= 0 && a <= line){
        for (int i = 0; i < column; ++i)
            data[a][i] *= temp;
        
    }
}

void multymergelines(int line1, int line2, double temp){
    if (line1 >= 0 && line1 <= line && line2 >= 0 && line2 <= line){
        for (int i = 0; i < column; ++i){
            data[line1][i] += data[line2][i] * temp;

        }
    }
}

    matrix(int a, int b) : line(a), column(b), data(nullptr) {
    data = new double*[a];
    for (size_t i = 0; i < a; ++i) {
        data[i] = new double[b];
    }
    }

    ~matrix() {
        for (int i = 0; i < line; ++i) {
            delete[] data[i];
        }
        delete[] data;
    }
    
    void inprogfile(const string& filename, int a = 1) {
    ifstream ifs(filename);
    if (!ifs.is_open()) {
        cerr << "Файл не был открыт." << endl;
        return;
    }

    cout << "Чтение данных из файла ...\n";

    if (!(ifs >> line >> column)) {
        cerr << "Ошибка чтения размеров матрицы." << endl;
        return;
    }

    data = new double*[line];
    for (int i = 0; i < line; ++i) {
        data[i] = new double[column];
    }

    for (int i = 0; i < line; ++i) {
        for (int j = 0; j < column; ++j) {
            if (!(ifs >> data[i][j])) {
                cerr << "Ошибка чтения элемента [" << i << "][" << j << "]" << endl;
                return;
            }
        }
    }

    ifs.close();
}


    void outprog() {
        if (data == nullptr) {
            cerr << "Матрица пуста, нечего выводить." << endl;
            return;
        }

        for (int i = 0; i < line; ++i) {
            for (int j = 0; j < column; ++j) {
                cout << data[i][j] << " ";
            }
            cout << endl;
        }
    }

    void inprog() {
        for (size_t i = 0; i < line; ++i)
            for (size_t j = 0; j < column; ++j){
                cin >> data[i][j];
            }   
    }

    void outprogfile(const string& filename) {
    ofstream file(filename);
    if (!file.is_open()) {
        cerr << "Ошибка: не удалось открыть файл " << filename << " для записи." << endl;
        return;
    }

    file << "Размеры матрицы: " << line << " x " << column << endl;
    for (size_t i = 0; i < line; ++i) {
        for (size_t j = 0; j < column; ++j) {
            file << data[i][j] << " ";
        }
        file << endl;
    }

    file.close();
    }

    matrix operator+(const matrix& other) const{
        if (column != other.column || line != other.line){
            cout << "Невозможно сложить, т.к. размеры матриц отличаются." << endl;
        }
        matrix ans(line, column);
        for (size_t i = 0; i < line; ++i){
            for (size_t j = 0; j < column; ++j){
                ans.data[i][j] = data[i][j] + other.data[i][j];
            }
        }
        return ans;
    }
    matrix operator-(const matrix& other) const{
        if (column != other.column || line != other.line){
            cout << "Невозможно сложить, т.к. размеры матриц отличаются." << endl;
        }
        matrix ans(line, column);
        for (size_t i = 0; i < line; ++i){
            for (size_t j = 0; j < column; ++j){
                ans.data[i][j] = data[i][j] - other.data[i][j];
            }
        }
        return ans;
    }
    matrix operator*(double a){
        matrix ans(line, column);
        for (size_t i = 0; i < line; ++i){
            for (size_t j = 0; j < column; ++j){
                ans.data[i][j] = data[i][j] * a; 
            }
        }
        return ans;
    } 
    matrix operator*(const matrix& other) const{
        int temp = 0, k = 0;
        matrix ans( line, other.column);
        if (column == other.line){ 
            for (size_t i = 0; i < line; i++) {
                for (size_t j = 0; j < other.column; j++) {
                    ans.data[i][j] = 0;
                    for (size_t k = 0; k < column; k++)
                        ans.data[i][j] += data[i][k] * other.data[k][j];
                }
            }   
        }
    return ans;
    }

void Gaus() {
    for (size_t i = 0; i < line; ++i) {
        size_t maxline = i;
        for (size_t k = i + 1; k < line; ++k) {
            if (fabs(data[k][i]) > fabs(data[maxline][i])) {
                maxline = k;
            }
        }
        swap(i, maxline);
        double temp = data[i][i];
        if (fabs(temp) < 1e-9) continue;
        for (size_t j = 0; j < column; ++j) {
            data[i][j] /= temp;
            if (fabs(data[i][j]) < 1e-9) {
                data[i][j] = 0.0;
            }
        }

        for (size_t k = 0; k < line; ++k) {
            if (k != i) {
                double coef = data[k][i];
                for (size_t j = 0; j < column; ++j) {
                    data[k][j] -= coef * data[i][j];
                    if (fabs(data[k][j]) < 1e-9) {
                        data[k][j] = 0.0;
                    }
                }
            }
        }
    }
    }
    void ansSLU() {
    int basic[column];
    for (int i = 0; i < column; ++i) {
        basic[i] = -1;
    }
   
    for (int i = 0; i < line; ++i) {
        for (int j = 0; j < column; ++j) {
            if (abs(data[i][j]) > 1e-9) { 
                basic[j] = i;
                break;
            }
        }
    }

    cout << "Общее решение в параметрической форме:\n";
    for (int j = 0; j < column - 1; ++j) { 
        if (basic[j] != -1) {
            cout << "x" << j + 1 << " = " << data[basic[j]][column - 1];
            for (int k = 0; k < column - 1; ++k) {
                if (basic[k] == -1 && abs(data[basic[j]][k]) > 1e-9) {
                    cout << " - (" << data[basic[j]][k] << ") * x" << k + 1;
                }
            }
            cout << endl;
        } else {
            cout << "x" << j + 1 << " = x" << j + 1 << endl; 
        }
    }
    }

    void ansSLUfile(const string& filename) {
    ofstream file(filename);
    if (!file.is_open()) {
        cerr << "Ошибка: не удалось открыть файл " << filename << " для записи." << endl;
        return;
    }

    int basic[column]; 
    for (int i = 0; i < column; ++i) {
        basic[i] = -1; 
    }

    
    for (int i = 0; i < line; ++i) {
        for (int j = 0; j < column; ++j) {
            if (fabs(data[i][j]) > 1e-9) { 
                basic[j] = i;
                break;
            }
        }
    }

    file << "Общее решение в параметрической форме:\n";
    for (int j = 0; j < column - 1; ++j) { 
        if (basic[j] != -1) {
            file << "x" << j + 1 << " = " << data[basic[j]][column - 1];
            for (int k = 0; k < column - 1; ++k) {
                if (basic[k] == -1 && fabs(data[basic[j]][k]) > 1e-9) {
                    file << " - (" << data[basic[j]][k] << ") * x" << k + 1;
                }
            }
            file << endl;
        } else {
            file << "x" << j + 1 << " = x" << j + 1 << endl; 
        }
    }

    file.close();
}

    bool operator==(const matrix& other) const{
        if (line != other.line || column != other.column){
            return 0;
        }
        for (int i = 0; i < this->line; ++i){
            for (int j = 0; j < this->column; ++j){
                if (data[i][j] != other.data[i][j])
                    return 0;
            }
        }
        return 1;
    }
    bool operator!=(const matrix& other) const{
        if (line != other.line || column != other.column){
            return 1;
        }
        for (int i = 0; i < line; ++i){
            for (int j = 0; j < column; ++j){
                if (data[i][j] != other.data[i][j])
                    return 1;
            }
        }
        return 0;
    }

    double determinant() {
    if (line != column) {
        cerr << "Определитель можно найти только для квадратных матриц." << endl;
        return 0;
    }
    matrix temp(*this);
    double det = 1; 
    for (int i = 0; i < line; ++i) {
        int maxRow = i;
        for (int k = i + 1; k < line; ++k) {
            if (abs(temp.data[k][i]) > abs(temp.data[maxRow][i])) {
                maxRow = k;
            }
        }
        if (abs(temp.data[maxRow][i]) < 1e-9) {
            return 0;
        }
        if (maxRow != i) {
            temp.swap(i, maxRow);
            det = -det;
        }
        det *= temp.data[i][i];
        for (int k = i + 1; k < line; ++k) {
            double coef = temp.data[k][i] / temp.data[i][i];
            for (int j = i; j < column; ++j) {
                temp.data[k][j] -= coef * temp.data[i][j];
            }
        }
    }
    return det;
    }   

    matrix inverse() {
    if (line != column) {
        cout << "Обратная матрица существует только для квадратных матриц." << endl;
        throw runtime_error("Обратная матрица существует только для квадратных матриц.");
    }

    matrix temp(*this);
    matrix inv(line, column);
    for (int i = 0; i < line; ++i) {
        for (int j = 0; j < column; ++j) {
            inv.data[i][j] = (i == j) ? 1.0 : 0.0;
        }
    }

    for (int i = 0; i < line; ++i) {
        int maxline = i;
        for (int k = i + 1; k < line; ++k) {
            if (abs(temp.data[k][i]) > abs(temp.data[maxline][i])) {
                maxline = k;
            }
        }

        if (fabs(temp.data[maxline][i]) < 1e-9) {
            cerr << "Матрица не имеет обратной." << endl;
            throw runtime_error("Матрица не имеет обратной.");
        }

        temp.swap(i, maxline);
        inv.swap(i, maxline);

        double diagElement = temp.data[i][i];
        for (int j = 0; j < column; ++j) {
            temp.data[i][j] /= diagElement;
            inv.data[i][j] /= diagElement;
        }

        for (int k = 0; k < line; ++k) {
            if (k == i) continue;
            double coef = temp.data[k][i];
            for (int j = 0; j < column; ++j) {
                temp.data[k][j] -= coef * temp.data[i][j];
                inv.data[k][j] -= coef * inv.data[i][j];
            }
        }
    }

    return inv;
    }

    matrix operator!() {
    return inverse();
    }

    void checkans(matrix& A, matrix& B, matrix& X) {
    if (A.line != A.column) {
        cerr << "Матрица A должна быть квадратной для решения уравнения." << endl;
        return;
    }
    if (A.line != B.line) {
        cerr << "Количество строк матриц A и B должно совпадать." << endl;
        return;
    }

    matrix temp(A.line, A.column + 1);

    for (int i = 0; i < A.line; ++i) {
        for (int j = 0; j < A.column; ++j) {
            temp.data[i][j] = A.data[i][j];
        }
        temp.data[i][A.column] = B.data[i][0];
    }

    temp.Gaus();

    for (int i = 0; i < X.line; ++i) {
        X.data[i][0] = temp.data[i][A.column];
    }
    }
    void solveMatrixEquation(matrix& A, matrix& B, matrix& X) {
    if (A.line != B.line) {
        cerr << "Количество строк матриц A и B должно совпадать." << endl;
        return;
    }

    matrix augmented(A.line, A.column + 1);  
    for (int i = 0; i < A.line; ++i) {
        for (int j = 0; j < A.column; ++j) {
            augmented.data[i][j] = A.data[i][j];
        }
        augmented.data[i][A.column] = B.data[i][0]; 
    augmented.Gaus();

    for (int i = 0; i < X.line; ++i) {
        X.data[i][0] = augmented.data[i][A.column];
    }
    for (int col = 1; col < B.column; ++col) {
        for (int i = 0; i < A.line; ++i) {
            augmented.data[i][A.column] = B.data[i][col];
        }

        augmented.Gaus();

        for (int i = 0; i < X.line; ++i) {
            X.data[i][col] = augmented.data[i][A.column];
        }
    }
    }
    }
    void checkSolution(const matrix& A, const matrix& X, const matrix& B) {
    matrix result(A.line, B.column);
    for (int col = 0; col < B.column; ++col) {
        for (int i = 0; i < A.line; ++i) {
            result.data[i][col] = 0;
            for (int j = 0; j < A.column; ++j) {
                result.data[i][col] += A.data[i][j] * X.data[j][col];
            }
        }
    }
    bool isValid = true;
    for (int i = 0; i < A.line; ++i) {
        for (int j = 0; j < B.column; ++j) {
            if (fabs(result.data[i][j] - B.data[i][j]) > 1e-9) {
                isValid = false;
                cout << "Уравнение [" << i + 1 << "][" << j + 1 << "] не выполнено. Получено: " 
                     << result.data[i][j] << ", ожидалось: " << B.data[i][j] << endl;
            }
        }
    }
    if (isValid) {
        cout << "Решение системы уравнений проверено и корректно." << endl;
    } 
    else {
        cout << "Решение системы уравнений некорректно." << endl;
    }
    }
    bool is_basic(const matrix& A, int x, int y){
        int temp;
        for (int i = 0; i < y; ++i){
            if (A.data[x][i] == 1){
                temp = i;
                break;
            }
        }
        if (temp == y)
            return 1;
        else 
        return 0;
    }
    void form_b(const matrix& B, const matrix& A, int x, int y){
        int t = 0;
        for(int i = 0; i < x; ++i){
            B.data[0][t] = A.data[i][y];
            t ++;
        }
    }

};


void inprogfileMultiple(const string& filename, matrix& mat1, matrix& mat2) {
    ifstream file(filename);
    if (!file.is_open()) {
        cerr << "Файл не был открыт." << endl;
        return;
    }

    
    if (!(file >> mat1.line >> mat1.column)) {
        cerr << "Ошибка чтения размеров первой матрицы." << endl;
        return;
    }

    mat1.data = new double*[mat1.line];
    for (int i = 0; i < mat1.line; ++i) {
        mat1.data[i] = new double[mat1.column];
        for (int j = 0; j < mat1.column; ++j) {
            if (!(file >> mat1.data[i][j])) {
                cerr << "Ошибка чтения элемента [" << i << "][" << j << "] первой матрицы." << endl;
                return;
            }
        }
    }

    
    if (!(file >> mat2.line >> mat2.column)) {
        cerr << "Ошибка чтения размеров второй матрицы." << endl;
        return;
    }

    mat2.data = new double*[mat2.line];
    for (int i = 0; i < mat2.line; ++i) {
        mat2.data[i] = new double[mat2.column];
        for (int j = 0; j < mat2.column; ++j) {
            if (!(file >> mat2.data[i][j])) {
                cerr << "Ошибка чтения элемента [" << i << "][" << j << "] второй матрицы." << endl;
                return;
            }
        }
    }

    file.close();
    
}

int menu()
{       
    int a, temp1, temp2;
    cout << "Выберете действие:"<< endl;
    cout << "   1) Сложение матриц" << endl;
    cout << "   2) Вычитание матриц" << endl;
    cout << "   3) Умножение матрицы на число" << endl;
    cout << "   4) Умножение матриц" << endl;
    cout << "   5) Решение СЛУ" << endl;
    cout << "   6) Проверка на равенство матриц" << endl;
    cout << "   7) Найти определитель матрицы" << endl;
    cout << "   8) Найти обратную матрицу" << endl;
    cout << "   9) Решить матричное уравнение" << endl;
    cin >> a;
    cout << "Выберете действие:"<< endl;
    cout << "    1) Взять данные для задачи из файла" << endl;
    cout << "    2) Ввести данные для задачи из терминала" << endl;
    cin >> temp1;
    cout << "Выберете действие:"<< endl;
    cout << "    1) Вывести ответ в файл" << endl;
    cout << "    2) Ввести ответ в терминал" << endl;
    cin >> temp2;
    int x, y, x1, y1, u, vozhproverk;
    double c, det;

switch (a)
{
case 1:
    if (temp1 == 1){
        matrix A(0, 0);
        matrix B(0, 0);
        inprogfileMultiple("23.txt", A, B);
        matrix C = A + B;
        if (temp2 == 1) {
            C.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
        } else {
            C.outprog();
        }
    }
    else{
        cout << "Введите количество строк и столбцов у 1 матрицы: "<< endl;
        cin >> x >> y;
        cout << "Введите числа из 1 матрицы" << endl;
        matrix A(x, y);
        A.inprog();
        cout << "Введите количество строк и столбцов у 2 матрицы: "<< endl;
        cin >> x >> y;
        cout << "Введите числа из 2 матрицы" << endl;
        matrix B(x, y);
        B.inprog();
        matrix C = A + B;
        if (temp2 == 1) {
            C.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
        } else {
            C.outprog();
        }
    }
    break;
case 2:
    if (temp1 == 1){
        matrix A(0, 0);
        matrix B(0, 0);
        inprogfileMultiple("23.txt", A, B);
        matrix C = A - B;
        if (temp2 == 1) {
            C.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
        } else {
            C.outprog();
        }
    }
    else{
        cout << "Введите количество строк и столбцов у 1 матрицы: "<< endl;
        cin >> x >> y;
        cout << "Введите числа из 1 матрицы" << endl;
        matrix A(x, y);
        A.inprog();
        cout << "Введите количество строк и столбцов у 2 матрицы: "<< endl;
        cin >> x >> y;
        cout << "Введите числа из 2 матрицы" << endl;
        matrix B(x, y);
        B.inprog();
        matrix C = A - B;
        if (temp2 == 1) {
            C.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
        } else {
            C.outprog();
        }
    }
    break;
case 3:
    if (temp1 == 1){
        matrix A(0, 0);
        A.inprogfile("23.txt");
        cout << "Введите число, на которое хотите умножить матрицу" << endl;
        cin >> c;
        matrix C = A * c;
        if (temp2 == 1) {
            C.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
        } else {
            C.outprog();
        }
    }
    else{
        cout << "Введите количество строк и столбцов у матрицы: "<< endl;
        cin >> x >> y;
        cout << "Введите числа из матрицы" << endl;
        matrix A(x, y);
        A.inprog();
        cout << "Введите число, на которое хотите умножить матрицу" << endl;
        cin >> c;
        matrix C = A * c;
        if (temp2 == 1) {
            C.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
        } else {
            C.outprog();
        }
    }
    break;
case 4:
    if (temp1 == 1){
        matrix A(0, 0);
        matrix B(0, 0);
        inprogfileMultiple("23.txt", A, B);
        matrix C = A * B;
        if (temp2 == 1) {
            C.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
        } else {
            C.outprog();
        }
    }
    else {
        cout << "Введите количество строк и столбцов у 1 матрицы: "<< endl;
        cin >> x >> y;
        cout << "Введите числа из 1 матрицы" << endl;
        matrix A(x, y);
        A.inprog();
        cout << "Введите количество строк и столбцов у 2 матрицы: "<< endl;
        cin >> x >> y;
        cout << "Введите числа из 2 матрицы" << endl;
        matrix B(x, y);
        B.inprog();
        matrix C = A * B;
        if (temp2 == 1) {
            C.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
        } else {
            C.outprog();
        }
    }
    break;
case 5:
    if (temp1 == 1) {
        matrix A(0, 0);
        matrix B(x, 1);
        bool K = A.is_basic(A, x, y);
        if (K == 1)
            vozhproverk = 0;
        
        else 
            vozhproverk = 1;
        A.inprogfile("23.txt");
        A.Gaus();
        if (temp2 == 1) {
            A.ansSLUfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
            if (vozhproverk == 1){
            cout << "Проверить решение?" << endl;
            cout << "   1)Да" << endl;
            cout << "   2)Нет"<< endl;
            cin >> u;
            if (u == 1){
                matrix X(A.column, 1);
                B.form_b(B, A, x, y);
                A.checkSolution(A, X, B);  
            }
            else 
                break;
            }
            else {
                cout << "Ответ невозможно проверить, т.к. последний элеменит из последней строки является главным" << endl;
                break;
            }
        }
        else{
            A.ansSLU();
            if (vozhproverk == 1){
            cout << "Проверить решение?" << endl;
            cout << "   1)Да" << endl;
            cout << "   2)Нет"<< endl;
            cin >> u;
            if (u == 1){
                matrix X(A.column, 1);
                B.form_b(B, A, x, y);
                A.checkSolution(A, X, B);  
            }
            else 
                break;
            }
            else {
                cout << "Ответ невозможно проверить, т.к. последний элеменит из последней строки является главным" << endl;
                break;
            }
        }
    }
    else {
        cout << "Введите количество строк и столбцов: " << endl;
        cin >> x >> y;
        matrix A(x, y);
        matrix B(x, 1);
        cout << "Введите числа из матрицы" << endl;
        A.inprog();
        A.Gaus();
        bool K = A.is_basic(A, x, y);
        if (K == 1)
            vozhproverk = 0;
        
        else 
            vozhproverk = 1;
        if (temp2 == 1) {
            A.ansSLUfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
            if (vozhproverk == 1){
            cout << "Проверить решение?" << endl;
            cout << "   1)Да" << endl;
            cout << "   2)Нет"<< endl;
            cin >> u;
            if (u == 1){
                matrix X(A.column, 1);
                B.form_b(B, A, x, y);
                A.checkSolution(A, X, B);  
            }
            else 
                break;
            }
            else {
                cout << "Ответ невозможно проверить, т.к. последний элеменит из последней строки является главным" << endl;
                break;
            }
        }
        else{
            A.ansSLU();
            if (vozhproverk == 1){
            cout << "Проверить решение?" << endl;
            cout << "   1)Да" << endl;
            cout << "   2)Нет"<< endl;
            cin >> u;
            if (u == 1){
                matrix X(A.column, 1);
                B.form_b(B, A, x, y);
                A.checkSolution(A, X, B);  
            }
            else 
                break;
            }
            else {
                cout << "Ответ невозможно проверить, т.к. последний элеменит из последней строки является главным" << endl;
                break;
            }
            
        }
    }
    break;
case 6:
    if (temp1 == 1) {
        matrix A(0, 0);
        matrix B(0, 0);
        inprogfileMultiple("23.txt", A, B);
        if (A == B) {
            if (temp2 == 1) {
                ofstream file("output.txt");
                file << "Матрицы равны";
                file.close();
                cout << "Результат записан в файл output.txt" << endl;
            } 
            else {
                cout << "Матрицы равны" << endl;
            }
        } 
        else {
            if (temp2 == 1) {
                ofstream file("output.txt");
                file << "Матрицы не равны";
                file.close();
                cout << "Результат записан в файл output.txt" << endl;
            } 
            else {
                cout << "Матрицы не равны" << endl;
            }
        }
    } 
    else {
        cout << "Введите количество строк и столбцов у 1 матрицы: " << endl;
        cin >> x >> y;
        matrix A(x, y);
        cout << "Введите числа для 1 матрицы:" << endl;
        A.inprog();
        cout << "Введите количество строк и столбцов у 2 матрицы: " << endl;
        cin >> x1 >> y1;
        matrix B(x1, y1);
        cout << "Введите числа для 2 матрицы:" << endl;
        B.inprog();

        if (A == B) {
            if (temp2 == 1) {
                ofstream file("output.txt");
                file << "Матрицы равны";
                file.close();
                cout << "Результат записан в файл output.txt" << endl;
            } else {
                cout << "Матрицы равны" << endl;
            }
        } 
        else {
            if (temp2 == 1) {
                ofstream file("output.txt");
                file << "Матрицы не равны";
                file.close();
                cout << "Результат записан в файл output.txt" << endl;
            } 
            else {
                cout << "Матрицы не равны" << endl;
            }
        }
    }
    break;
case 7:
    if (temp1 == 1) {
        matrix A(0, 0);
        A.inprogfile("23.txt");
        det = A.determinant();
        if (temp2 == 1){
            ofstream file("output.txt");
            file << det << endl;
            cout << "Результат записан в файл output.txt" << endl;
            file.close();
        }
        else {
            cout << det << endl;
        }
    }
    else {
        cout << "Введите количество строк и столбцов у матрицы: " << endl;
        cin >> x >> y;
        matrix A(x, y);
        cout << "Введите числа из матрицы:" << endl;
        A.inprog();
        if (temp2 == 1){
            ofstream file("output.txt");
            file << det << endl;
            cout << "Результат записан в файл output.txt" << endl;
            file.close();
        }
        else {
            cout << det << endl;
        }
    }
    break;
case 8:
    if (temp1 == 1){
        matrix A(0, 0);
        A.inprogfile("23.txt");
        matrix C = !A;
        if (temp2 == 1) {
            C.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
        }
        else{
            C.outprog();
        }
    }
    else{
        cout << "Введите количество строк и столбцов у матрицы: " << endl;
        cin >> x >> y;
        matrix A(x, y);
        cout << "Введите числа из матрицы:" << endl;
        A.inprog();
        A.inverse();
        matrix C = !A;
        if (temp2 == 1) {
            C.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
        }
        else{
            C.outprog();
        }
    }
    break;
case 9:
    if (temp1 == 1){
        matrix A(0, 0);
        matrix B(0, 0);
        inprogfileMultiple("23.txt", A, B);
        if (A.line != A.column) {
            cerr << "Уравнение может быть решено только для квадратной матрицы A." << endl;
            break;
        }
        matrix X(A.column, 1);
        X.solveMatrixEquation(A, B, X);
        if (temp2 == 1) {
            X.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
            cout << "Проверить решение?" << endl;
            cout << "   1)Да" << endl;
            cout << "   2)Нет"<< endl;
            cin >> u;
            if (u == 1)
                A.checkSolution(A, X, B);
            else 
                break;
            
            
        } else {
            X.outprog();
            cout << "Проверить решение?" << endl;
            cout << "   1)Да" << endl;
            cout << "   2)Нет"<< endl;
            cin >> u;
            if (u == 1)
                A.checkSolution(A, X, B);
            else 
                break;
            
        }
    }
    else{
        cout << "Введите количество строк и столбцов у матрицы A: " << endl;
        cin >> x >> y;
        matrix A(x, y);
        cout << "Введите числа из матрицы A:" << endl;
        A.inprog();

        cout << "Введите количество строк у матрицы B (столбец): " << endl;
        cin >> x >> y;
        if (y != 1) {
            cerr << "Матрица B должна быть вектор-столбцом." << endl;
            break;
        }
        matrix B(x, y);
        cout << "Введите числа из матрицы B:" << endl;
        B.inprog();
        if (A.line != B.line) {
            cerr << "Количество строк матриц A и B должно совпадать." << endl;
            break;
        }

        matrix X(A.column, 1);
        X.solveMatrixEquation(A, B, X);

        if (temp2 == 1) {
            X.outprogfile("output.txt");
            cout << "Результат записан в файл output.txt" << endl;
            cout << "Проверить решение?" << endl;
            cout << "   1)Да" << endl;
            cout << "   2)Нет"<< endl;
            cin >> u;
            if (u == 1)
                A.checkSolution(A, X, B);
            else 
                break;
        } 
        else 
            X.outprog();
            cout << "Проверить решение?" << endl;
            cout << "   1)Да" << endl;
            cout << "   2)Нет"<< endl;
            cin >> u;
            if (u == 1)
                A.checkSolution(A, X, B);
            else 
                break;
        
    }
    break;

}
    return 0;
}
