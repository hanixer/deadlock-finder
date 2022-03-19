
package mpi.corrbench;
import mpi.MPI;

public class MpiSendRecvConditions {
    public static void main(String[] args) {
        int rank = MPI.COMM_WORLD.Rank();
        int[] buffer = {0, 2, 3, 4};
        if (rank == 0) {
            MPI.COMM_WORLD.Send(buffer, 0, 4, MPI.INT, 1, tag);
            MPI.COMM_WORLD.Recv(buffer, 0, 4, MPI.INT, 1, tag);
        } else if (rank == 1) {
            MPI.COMM_WORLD.Recv(buffer, 0, 4, MPI.INT, 0, tag);
            int x = buffer[1];
            if (x == 1) {
                MPI.COMM_WORLD.Send(buffer, 0, 4, MPI.INT, 0, tag);
            } else {
                MPI.COMM_WORLD.Recv(buffer, 0, 4, MPI.INT, 0, tag);
            }
        }
    }
}
