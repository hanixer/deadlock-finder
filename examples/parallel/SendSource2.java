import mpi.MPI;
import mpi.Status;

public class SendSource {
    public static void main(String[] args) {
        MPI.Init(args);

        int[] buf = new int[10];

        Status status = MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, MPI.ANY_SOURCE, 0);
        MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, status.source, 0);

        MPI.Finalize();
    }
}
