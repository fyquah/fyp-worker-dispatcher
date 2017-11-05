for i in $(seq 0 11); do
  for j in $(seq 0 15); do
    jbuilder exec -- display diff-results \
      ../experiments/results/$i/results/results_$j.sexp \
      ../experiments/results/$(echo "$i + 1" | bc)/results/results_$j.sexp
  done
done
