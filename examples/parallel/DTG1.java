import mpi.MPI;

import java.util.List;

public class DTG {
    public static void main(String[] args) {
        MPI.Init(args);

        int rank = MPI.COMM_WORLD.Rank();

        int[] buf = new int[10];

        if (rank == 0) {
            MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, MPI.ANY_SOURCE, 0);
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, 3, 0);
            MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, MPI.ANY_SOURCE, 0);
        } else if (rank == 1) {
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, 0, 0);
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, 3, 0);
        } else if (rank == 2) {
            MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, MPI.ANY_SOURCE, 0);
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, 0, 0);
        } else if (rank == 3) {
            MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, 1, 0);
            MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, 0, 0);
        } else if (rank == 4) {
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, 2, 0);
        }

        MPI.Finalize();
    }
}
