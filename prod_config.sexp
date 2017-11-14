((num_runs 10)
 (worker_configs
   ((Ssh_worker
     ((rundir /home/fyquah/worker-rundir/0)
      (user fyquah)
      (hostname 192.168.0.10)))
    (Ssh_worker
     ((rundir /home/fyquah/worker-rundir/0)
      (user fyquah)
      (hostname 192.168.0.11)))
    (Ssh_worker
     ((rundir /home/fyquah/worker-rundir/0)
      (user fyquah)
      (hostname 192.168.0.12)))
    )))
