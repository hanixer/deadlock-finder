import mpi.MPI;
import mpi.Status;

public class SendSource {
    static int rank;
    static int size;

    public static void main(String[] args) {
        MPI.Init(args);

        rank = MPI.COMM_WORLD.Rank();
        size = MPI.COMM_WORLD.Size();

        int[] buf = new int[10];

        if (rank == 0) {
            Status status = MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, MPI.ANY_SOURCE, 0);
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, status.source, 0);
            status = MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, MPI.ANY_SOURCE, 0);
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, status.source, 0);
        } else if (rank == 1) {
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, 0, 0);
            MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, 0, 0);
        } else if (rank == 2) {
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, 0, 0);
            MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, 0, 0);
        }

        System.out.println(rank);

        MPI.Finalize();
    }
}
