
package mpi.corrbench;
import mpi.MPI;

public class MpiSendRecvConditions5 {
    public static void main(String[] args) {
        int rank = MPI.COMM_WORLD.Rank();
        int[] buffer = {0, 2, 3, 4};
        if (rank == 0) {
            int x = buffer[1];
            MPI.COMM_WORLD.Recv(buffer, 0, 4, MPI.INT, 1, tag);
            if (x == 1) {
                MPI.COMM_WORLD.Recv(buffer, 0, 4, MPI.INT, 2, tag);
            } else {
                MPI.COMM_WORLD.Recv(buffer, 0, 4, MPI.INT, 3, tag);
            }
            MPI.COMM_WORLD.Recv(buffer, 0, 4, MPI.INT, 4, tag);
        }
    }
}
