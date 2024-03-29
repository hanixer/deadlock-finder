
package mpi.corrbench;
import mpi.MPI;

public class MpiRecvDeadlock1 {
    public static void main(String[] args) {
        MPI.Init(args);

        int rank = MPI.COMM_WORLD.Rank();
        int[] buffer = {0, 2, 3, 4};

        int tag = 321;

        try {
            if (rank == 0) {
                MPI.COMM_WORLD.Recv(buffer, 0, 4, MPI.INT, 1, tag);
                MPI.COMM_WORLD.Send(buffer, 0, 4, MPI.INT, 1, tag);
            } else if (rank == 1) {
                MPI.COMM_WORLD.Recv(buffer, 0, 4, MPI.INT, 0, tag);
                MPI.COMM_WORLD.Send(buffer, 0, 4, MPI.INT, 0, tag);
            }
        } finally {
            MPI.Finalize();
        }
    }
}
