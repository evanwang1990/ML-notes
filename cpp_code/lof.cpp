#include <iostream>
#include "vector"
#include <algorithm> 
#include "math.h"
using namespace std;

float dist(vector<float> x, vector<float> y);
vector<int> kDistNN(vector<float> dist, int k);

int main()
{
	float a_x[4] = {1, 2, 3, 4};
	float a_y[4] = {2, 6, 4, 5};
	vector<float> x (a_x, a_x + 4);
	vector<float> y (a_y, a_y + 4);
	cout << dist(x, y)<<endl;
	vector<int> small = kDistNN(y, 3);
	for(int i = 0; i < 3; ++i)
	{
		cout<<small[i]<<endl;
	}
	return 0;
}





float dist(vector<float> x, vector<float> y)
{
	float res;
	for(int i = 0; i < x.size(); ++i)
	{
		res += pow(x[i] - y[i], 2);
	}
	res = sqrt(res);
	return res;
}

vector<int> kDistNN(vector<float> dist, int k)
{
	vector<float> tmp(dist.size());
	vector<int> res;
	copy(dist.begin(), dist.end(), tmp.begin());
	nth_element(tmp.begin(), tmp.begin() + k, tmp.end());
	for(int i = 0; i < dist.size(); ++i)
	{
		if(dist[i] <= tmp[k-1]) res.push_back(i);
	}
	return res;
}
