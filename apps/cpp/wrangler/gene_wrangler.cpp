#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <map>
#include <bitset>
#include "omp.h"
#include <time.h>
#include <math.h>
#include <sys/time.h>

using namespace std;

#define FILE_WIDTH 4

int THREADS = 1;
int deleteClipperAndCut(string *** table, int _length, int _width); 
int gene_processing(string *** table, int length, int width) ;

void goodbye(string msg) {
    cout << msg << endl;
    exit(1);
}

// this version tries to more closely mirror what
// wrangler does

int main(int argc, char *argv[]) {
    printf("Hello World\n");
    if (argc < 4) goodbye("pass in the fastq files and number of THREADS, yo");
    THREADS = atoi(argv[3]);
    omp_set_num_threads(THREADS);
    ifstream fastq ; //= argv[0];  
    fastq.open (argv[1], ios::in);
    if (! fastq.is_open()) goodbye("This file is not open, yo.");
    string line;
    int file_length = -1; // -1 for EOF
    while( fastq.good() ) {
        getline(fastq, line);
        // do something
        file_length++;
    }
    fastq.close();
    printf("lines in file: %d\n", file_length);

    //string table[file_length][FILE_WIDTH];
    string **table = new string*[file_length];
    for(int i = 0; i < file_length; i++) table[i] = new string[FILE_WIDTH];
    fastq.open (argv[1], ios::in);
    if (! fastq.is_open()) goodbye("This file is not open, yo.");
    for(int i = 0; i < file_length; i++) {
        getline(fastq, line);
        stringstream ss(line);
        int line_width = 0;
        string token;
        vector<string> v;
        while(getline(ss, token, '\t')) {
            line_width++;
            v.push_back(token);
            //     cout << "elem: " << table[i][line_width] << endl;
        }
        if(line_width != 4) goodbye("Bad line");
        copy(v.begin(), v.end(), table[i]);
    }
    fastq.close();

    // wow that was painful

    cout << "gene_processing" << endl; 
    
    struct timeval myprofiler_start, myprofiler_end;
    gettimeofday(&myprofiler_start,NULL);
    int new_length = gene_processing(&table, file_length, FILE_WIDTH);
    gettimeofday(&myprofiler_end,NULL);
    
    printf("Total Time : %ld [us]\n", ((myprofiler_end.tv_sec * 1000000 + myprofiler_end.tv_usec) - (myprofiler_start.tv_sec * 1000000 + myprofiler_start.tv_usec)));

    cout << new_length << endl;
    // write to file
    ofstream out;
    out.open (argv[2], ios::out);
    for (int i = 0; i < new_length; i++) { 
        for (int j = 0; j < FILE_WIDTH-1; j++) {
            out << table[i][j] << "\t"; 
        } 
        out << table[i][FILE_WIDTH-1] << endl;
    }
    return 0;
}

//string g = "AGATCGGAAGAGCGGTTCAGCAGGAATGCCGAGACCGATCTCGTATGCCGTCTTCTGCTTG";
string g = "AGAT";//CGGAAGAGCGGTTCAGCAGGAATGCCGAGACCGATCTCGTATGCCGTCTTCTGCTTG";

bool clip(string seq) {
    int GENE_SIZE = g.size();
    if(seq.size() < GENE_SIZE) return false;
    bool match = true;
    for(int i = 0; i < g.size(); i++) {
        if (seq[i] != 'N' && seq[i] != g[i]) {
            match = false;
            break ;
        }
    }
    return match; 
}

int gene_processing(string *** table, int _length, int _width) { 
    int new_length = deleteClipperAndCut(table, _length, _width);
    return new_length;
}

int deleteClipperAndCut(string *** table, int _length, int _width) {  
    int i;

    vector<string *> **local_tables = (vector<string *> **)malloc(THREADS * sizeof(vector<string *> *));
    string **Table = *table;

    //struct timeval myprofiler_start, myprofiler_end;
    //gettimeofday(&myprofiler_start,NULL);

    #pragma omp parallel private(i)
    {
        int thread = omp_get_thread_num();
        vector<string *> *local_table = new vector<string *>();
        local_tables[thread] = local_table;
        // Is it fair to pre-allocate? 
        // This helps removing dynamic allocations and cache line false sharing
        local_table->reserve(1 + (_length-1)/THREADS); 

        #pragma omp for private(i) schedule(static)
        for(i=0; i<_length; i++) {
            if(!clip(Table[i][1])) {
                int sz = Table[i][1].size();
                if (sz >= 13) (*table)[i][1].erase(13,sz-13);
                //sbtstr creates a new string and assignment operator does deep copy, which does not scale with multi-core.
                //if (sz >= 13) (*table)[i][1] = Table[i][1].substr(13);
                local_table->push_back((*table)[i]);
            }
        } // end of omp for
    } // end of omp parallel

    //gettimeofday(&myprofiler_end,NULL);
    //printf("Filter Time : %ld [us]\n", ((myprofiler_end.tv_sec * 1000000 + myprofiler_end.tv_usec) - (myprofiler_start.tv_sec * 1000000 + myprofiler_start.tv_usec)));

    //gettimeofday(&myprofiler_start,NULL);
    
    //TODO: we could also do this part in parallel, but currently doesn't take that much time (10% of total)
    vector<string*> *new_table = new vector<string *>;
    for(i=0; i<THREADS; i++)
        new_table->insert(new_table->end(), local_tables[i]->begin(), local_tables[i]->end());
    
    //gettimeofday(&myprofiler_end,NULL);
    //printf("Combine Time : %ld [us]\n", ((myprofiler_end.tv_sec * 1000000 + myprofiler_end.tv_usec) - (myprofiler_start.tv_sec * 1000000 + myprofiler_start.tv_usec)));

    (*table) = &(*new_table)[0];
    return new_table->size();
}

