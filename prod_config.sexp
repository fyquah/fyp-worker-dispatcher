((num_runs 6)
 (worker_configs
   ((Ssh_worker
     ((rundir /home/fyquah/worker-rundir/0)
      (user fyquah)
      (hostname 192.168.0.10)
      (processor 0)))
    (Ssh_worker
     ((rundir /home/fyquah/worker-rundir/0)
      (user fyquah)
      (hostname 192.168.0.11)
      (processor 1)))
    (Ssh_worker
     ((rundir /home/fyquah/worker-rundir/0)
      (user fyquah)
      (hostname 192.168.0.12)
      (processor 2))))))
