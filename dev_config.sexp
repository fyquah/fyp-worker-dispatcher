((num_runs 3)
 (worker_configs
   ((Ssh_worker
     ((user fyquah)
      (rundir /home/fyquah/fyp/dev/worker-rundir/0)
      (hostname 0.0.0.0)))
    (Ssh_worker
     ((user fyquah)
      (rundir /home/fyquah/fyp/dev/worker-rundir/1)
      (hostname 0.0.0.0)))
    (Ssh_worker
     ((user fyquah)
      (rundir /home/fyquah/fyp/dev/worker-rundir/2)
      (hostname 0.0.0.0))))))
