#! /bin/bash

# rm -rf results
# mkdir results

echo "running pagerank app..."
export OMP_NUM_THREADS=1
export TIMER_PATH=results/pagerank.times
for ir in {1..5}
do
  echo "  run $ir/5"
  numactl --cpubind=0 --membind=0 -- graph/pagerank/pagerank /data/graph/soc-LiveJournal1.txt results/pagerank.$ir.pr 100 >results/pagerank.$ir.out 2>results/pagerank.$ir.err  
done
echo "done!"
echo ""

echo "running trianglecounting app..."
export OMP_NUM_THREADS=1
export TIMER_PATH=results/trianglecounting.times
for ir in {1..5}
do
  echo "  run $ir/5"
  numactl --cpubind=0 --membind=0 -- graph/trianglecounting/trianglecounting /data/graph/soc-LiveJournal1.txt >results/trianglecounting.$ir.out 2>results/trianglecounting.$ir.err  
done
echo "done!"
echo ""

echo "running kmeans app..."
export OMP_NUM_THREADS=1
export TIMER_PATH=results/kmeans.times
for ir in {1..5}
do
  echo "  run $ir/5"
  numactl --cpubind=0 --membind=0 -- ml/kmeans/kmeans /data/ml/kmeans/synthetic500k.dat /data/ml/kmeans/initmu_synth.dat >results/kmeans.$ir.out 2>results/kmeans.$ir.err  
done
echo "done!"
echo ""

echo "running logreg app..."
export OMP_NUM_THREADS=1
export TIMER_PATH=results/logreg.times
for ir in {1..5}
do
  echo "  run $ir/5"
  numactl --cpubind=0 --membind=0 -- ml/logreg/logreg /data/ml/kmeans/synthetic500k.dat /data/ml/kmeans/synthetic_vec.dat 1 >results/logreg.$ir.out 2>results/logreg.$ir.err  
done
echo "done!"
echo ""

echo "running gda app..."
export OMP_NUM_THREADS=1
export TIMER_PATH=results/gda.times
for ir in {1..5}
do
  echo "  run $ir/5"
  numactl --cpubind=0 --membind=0 -- ml/gda/GDA_arma /data/ml/gda/2048-1200x.dat /data/ml/gda/q1y.dat >results/gda.$ir.out 2>results/gda.$ir.err  
done
echo "done!"
echo ""

echo "running q1 app..."
cd ql
export OMP_NUM_THREADS=1
export TIMER_PATH=../results/q1.times
for ir in {1..5}
do
  echo "  run $ir/5"
  numactl --cpubind=0 --membind=0 -- scala Query1 /data/query/SF5 >../results/q1.$ir.out 2>../results/q1.$ir.err  
done
cd ..
echo "done!"
echo ""


echo "running gene app..."
export OMP_NUM_THREADS=1
export TIMER_PATH=results/gene.times
for ir in {1..5}
do
  echo "  run $ir/5"
  numactl --cpubind=0 --membind=0 -- wrangler/gene_wrangler /data/dna/1e8.fastq results/gene.$ir.gene 1 >results/gene.$ir.out 2>results/gene.$ir.err  
done
echo "done!"
echo ""

