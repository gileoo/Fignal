// Four1Test.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <vector>
#include <cmath>
#include <cassert>
#include <array>

using namespace std;

constexpr auto M_PI = 3.14159265358979323846;

void swap(double& a, double& b)
{
    double tmp = a;
    a = b;
    b = tmp;
}

void four1(double *data, unsigned long n, int isign)
{
    int nn, mmax, m, j, istep, i;
    double wtemp, wr, wpr, wpi, wi, theta, tempr, tempi;

    if (n < 2 || n & (n - 1))
        throw("n must be power of 2");

    nn = n << 1;
    j = 1;

    for (i = 1; i < nn; i += 2) {

        if (j > i) {
            swap(data[j-1], data[i-1]);
            swap(data[j], data[i]);
        }
        m = n;

        while (m >= 2 && j > m) {
            j -= m;
            m >>= 1;
        }

        j += m;
    }

    for (size_t i = 0; i < nn; i += 2)
        std::cout << "[]" << data[i] << ", " << data[i + 1] << std::endl;

    mmax = 2;

    while (nn > mmax) 
    {
        istep = mmax << 1;
        theta = isign * (2.0 * M_PI / mmax);
        wtemp = sin(0.5 * theta);
        wpr = -2.0 * wtemp * wtemp;

        wpi = sin(theta);
        wr = 1.0;
        wi = 0.0;
        for (m = 1; m < mmax; m += 2) 
        {
            for (i = m; i <= nn; i += istep) 
            {
                j = i + mmax;
                
                assert(j < nn);
                assert(j - 1 < nn);
                assert(j >= 0);
                assert(j -1 >= 0);
                assert(i < nn);
                assert(i - 1 < nn);
                assert(i >= 0);
                assert(i - 1 >= 0);

                tempr = wr * data[j-1] - wi * data[j];
                tempi = wr * data[j] + wi * data[j-1];

                data[j-1] = data[i-1] - tempr;
                data[j]   = data[i] - tempi;
                data[i-1] += tempr;
                data[i]   += tempi;
            }
            wr = (wtemp = wr) * wpr - wi * wpi + wr;
            wi = wi * wpr + wtemp * wpi + wi;
        }
        mmax = istep;
    }
}

void four1(vector<double>& data, int isign)
{
    four1(data.data(), data.size() / 2, isign);
}

void realft(vector<double>& data, int isign)
{
    int i, i1, i2, i3, i4;
    int n = data.size();
    double c1 = 0.5;
    double c2, h1r, h1i, h2r, h2i, wr, wi, wpr, wpi, wtemp;
    double theta = M_PI / double(n >> 1);
    if (isign == 1)
    {
        c2 = -0.5;
        four1( data, 1 );
    }
    else
    {
        c2 = 0.5;
        theta = -theta;
    }

    wtemp = sin(0.5 * theta);
    wpr = -2.0 * wtemp * wtemp;
    wpi = sin(theta);
    wr = 1.0 + wpr;
    wi = wpi;
    for (i = 1; i < (n >> 2); i++)
    {
        i2 = 1 + (i1 = i + i);
        i4 = 1 + (i3 = n - i1);
        h1r =  c1 * (data[i1] + data[i3]);
        h1i =  c1 * (data[i2] - data[i4]);
        h2r = -c2 * (data[i2] + data[i4]);
        h2i =  c2 * (data[i1] - data[i3]);
        data[i1] =  h1r + wr * h2r - wi * h2i;
        data[i2] =  h1i + wr * h2i + wi * h2r;
        data[i3] =  h1r - wr * h2r + wi * h2i;
        data[i4] = -h1i + wr * h2i + wi * h2r;

        wtemp = wr;
        wr = wr * wpr -    wi * wpi + wr;
        wi = wi * wpr + wtemp * wpi + wi;
    }

    if (isign == 1)
    {
        h1r = data[0];
        data[0] = h1r + data[1];
        data[1] = h1r - data[1];
    }
    else
    {
        h1r = data[0];
        data[0] = c1 * (h1r + data[1]);
        data[1] = c1 * (h1r - data[1]);
        four1(data, -1);
    }
}


void convlv(vector<double>& ans, const vector<double>& data, const vector<double>& respons, int isign)
{
    int i, no2, n = data.size(), m = respons.size();
    
    double mag2, tmp;
    vector<double> temp(n);

    temp[0] = respons[0];
    for (i = 1; i < (m + 1) / 2; i++)
    {
        temp[i]     = respons[i];
        temp[n - i] = respons[m - i];
    }
    for (i = (m + 1) / 2; i < n - (m - 1) / 2; i++)
        temp[i] = 0.0;
    for (i = 0; i < n; i++)
        ans[i] = data[i];

    realft(ans, 1);
    realft(temp, 1);   
   
    no2 = n >> 1;
    if (isign == 1)
    {
        for (i = 2; i < n; i += 2)
        {
            tmp = ans[i];
            ans[i]   = (ans[i]   * temp[i] - ans[i + 1] * temp[i + 1]) / no2;
            ans[i+1] = (ans[i+1] * temp[i] + tmp        * temp[i + 1]) / no2;
        }
        ans[0] = ans[0] * temp[0] / no2;
        ans[1] = ans[1] * temp[1] / no2;
    }
    else
    {
        for (i = 2; i < n; i += 2)
        {
            mag2 = temp[i] * temp[i] + temp[i + 1] * temp[i + 1];
            if (mag2 == 0.0)
                throw("Deconvolving  at reponse zero in convlv");
            tmp = ans[i];
            ans[i]     = (ans[i]     * temp[i] + ans[i + 1] * temp[i + 1]) / mag2 / no2;
            ans[i + 1] = (ans[i + 1] * temp[i] - tmp * temp[i + 1]) / mag2 / no2;
        }
        if (temp[0] == 0.0 || temp[1] == 0.0)
            throw("Deconvolving at response zero in convlv");
        ans[0] = ans[0] / temp[0] / no2;
        ans[1] = ans[1] / temp[1] / no2;
    }

    realft(ans, -1);
}

////////////////////////////////////////////
////////////////////////////////////////////






//void perform_fft(double** values, long length, int direction) {
void perform_fft(array<vector<double>, 2>& values, long length, int direction) {
   
    long exponent, i, i1, j, k, i2, l, l1, l2;
    double c1, c2, tx, ty, t1, t2, u1, u2, z;

    // Caculate the exponent
    exponent = 0;
    i = length;
    while (i >>= 1) {
        exponent++;
    }

    // Do the bit reversal
    i2 = length >> 1;
    j = 0;
    for (i = 0; i < (length - 1); i++) {
        if (i < j) {
            tx = values[0][i];
            ty = values[1][i];
            values[0][i] = values[0][j];
            values[1][i] = values[1][j];
            values[0][j] = tx;
            values[1][j] = ty;
        }
        k = i2;
        while (k <= j) {
            j -= k;
            k >>= 1;
        }
        j += k;
    }

    for (size_t i = 0; i < length; ++i)
        std::cout << "[]" << values[0][i] << ", " << values[1][i] << std::endl;

    // Compute the FFT
    c1 = -1.0;
    c2 = 0.0;
    l2 = 1;
    for (l = 0; l < exponent; l++) {
        l1 = l2;
        l2 <<= 1;
        u1 = 1.0;
        u2 = 0.0;
        for (j = 0; j < l1; j++) {
            for (i = j; i < length; i += l2) {
                i1 = i + l1;
                t1 = u1 * values[0][i1] - u2 * values[1][i1];
                t2 = u1 * values[1][i1] + u2 * values[0][i1];
                values[0][i1] = values[0][i] - t1;
                values[1][i1] = values[1][i] - t2;
                values[0][i] += t1;
                values[1][i] += t2;
            }
            z = u1 * c1 - u2 * c2;
            u2 = u1 * c2 + u2 * c1;
            u1 = z;
        }
        c2 = sqrt((1.0 - c1) / 2.0);
        if (direction == 1) {
            c2 = -c2;
        }
        c1 = sqrt((1.0 + c1) / 2.0);
    }

    
    // Scaling for forward transform
    if (direction == 1) {
        for (i = 0; i < length; i++) {
            values[0][i] /= length;
            values[1][i] /= length;
        }
    }
    
}







//////////////////////////////////////////
//////////////////////////////////////////


int main()
{


    cout << "Hello World!\n";

    const size_t N = 16;
    vector<double> dat(2 * N, 0.0);
    for (size_t i = 0; i < N; ++i)
        dat[2 * i] = cos(double(i) / double(N) * 4.0 * M_PI);
    
    for (size_t i = 0; i < N; ++i)
        printf( "%0.6f,\t%0.6f\n", dat[2 * i] , dat[2 * i + 1]);

    for (size_t i = 0; i < N; ++i)
        printf("%0.6f \t%0.6f ", dat[2 * i], dat[2 * i + 1]);
    
    cout << endl; 

    for (size_t i = 0; i < N; ++i)
        printf("%0.6f ", dat[2 * i] );

    cout << endl;

    cout << "------[ four1 ]------ " << endl;

    four1(dat.data(), N, 1);
    for (size_t i = 0; i < N; ++i)
        printf("%0.6f,\t%0.6f\n", dat[2 * i], dat[2 * i + 1]);


    cout << "--------------------- " << endl;
    vector<double> datR(N, 0.0);
    for (size_t i = 0; i < N; ++i)
        datR[i] = cos(double(i) / double(N) * 4.0 * M_PI);

    realft(datR, 1);
    for (size_t i = 0; i < N; ++i)
        printf("%0.6f\n", datR[i]);

    cout << "--------------------- " << endl;

    vector<double> D = { 0, 0, 0, 1, 1, 1, 0, 0};
    vector<double> K = { 0.5, 1.0, 0.25 };

    vector<double> A(D.size());

    convlv(A, D, K, 1);
    for (size_t i = 0; i < A.size(); ++i)
        printf("%0.6f\n", A[i]);

    cout << "--------------------- " << endl;
    cout << "--------------------- " << endl;



    array<vector<double>, 2> dat2 = { vector<double>(16, 0.0), vector<double>(16, 0.0) };
    for (size_t i = 0; i < 16; ++i)
        dat2[0][i] = cos(double(i) / double(N) * 4.0 * M_PI);


    for (size_t i = 0; i < 16; ++i)
        printf("%0.6f,\t%0.6f\n", dat2[0][i], dat2[1][i]);

    cout << "--------------------- " << endl;

    perform_fft(dat2, 16, -1);
    for (size_t i = 0; i < 16; ++i)
        printf("%0.6f,\t%0.6f\n", dat2[0][i], dat2[1][i]);


    return 0;
}

