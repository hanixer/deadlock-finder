import mpi.MPI;
import mpi.Status;

public class SendSource3 {
    public static void main(String[] args) {
        MPI.Init(args);

        int rank = MPI.COMM_WORLD.Rank();

        int[] buf = new int[10];

        if (rank == 0) {
            Status status = MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, MPI.ANY_SOURCE, 0);
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, status.source, 0);
        } else if (rank == 1) {
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, 0, 0);
            MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, 0, 0);
        }

        MPI.Finalize();
    }
}
