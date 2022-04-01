`package mpi.corrbench;
import mpi.MPI;

public class Loop1 {
    public static void main(String[] args) {
        int rank = MPI.COMM_WORLD.Rank();
        int[] buffer = {0, 2, 3, 4};
        int tag = 200;

        if (rank == 0) {
            for (int i = 0; i < 3; i++) {
                MPI.COMM_WORLD.Send(buffer, 0, 4, MPI.INT, 1, tag);
            }
        } else if (rank == 1) {
            for (int j = 0; j < 3; j++) {
                MPI.COMM_WORLD.Recv(buffer, 0, 4, MPI.INT, 0, tag);
            }
        }
    }
}
