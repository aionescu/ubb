package ro.arc.packageManager.server.server;

import ro.arc.packageManager.common.domain.Maintainer;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.networking.Message;
import ro.arc.packageManager.common.networking.utils.NetworkingHelper;
import ro.arc.packageManager.common.service.IMaintainerService;
import ro.arc.packageManager.common.service.IPackageMaintainerService;
import ro.arc.packageManager.common.service.IPackageService;
import ro.arc.packageManager.common.service.IPackageVersionService;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

public class TCPServer {
    private ExecutorService executorService;
    private int port;
    private Map<String,UnaryOperator<Message>> methodHandlers;
    private IMaintainerService maintainerService;
    private IPackageService packageService;
    private IPackageMaintainerService packageMaintainerService;
    private IPackageVersionService packageVersionService;

    public TCPServer(ExecutorService executorService, int port, IMaintainerService maintainerService,
                     IPackageService packageService, IPackageMaintainerService packageMaintainerService, IPackageVersionService packageVersionService) {
        this.executorService = executorService;
        this.port = port;
        this.methodHandlers = new HashMap<>();
        this.maintainerService = maintainerService;
        this.packageService = packageService;
        this.packageMaintainerService = packageMaintainerService;
        this.packageVersionService = packageVersionService;
        this.addMaintainerHandlers();
        this.addPackageHandlers();
        this.addPackageMaintainerHelpers();
        this.addPackageVersionHandlers();
    }



    public void addHandler(String methodName, UnaryOperator<Message> handler) {
        methodHandlers.put(methodName, handler);
    }

    public void startServer() {
        try (var serverSocket = new ServerSocket(this.port)) {
            System.out.println("server started; waiting for clients...");
            while (true) {
                Socket clientSocket = serverSocket.accept();
                System.out.println("client connected");
                executorService.submit(new ClientHandler(clientSocket));
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void addMaintainerHandlers()
    {
        this.addHandler("add maintainer", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 3)
                    throw new AppException("Impossible to create maintainer from message");
                this.maintainerService.addMaintainer(body.get(0), body.get(1), body.get(2));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
        });

        this.addHandler("delete maintainer", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 1)
                    throw new AppException("Different number of arguments to delete maintainer");
                this.maintainerService.deleteMaintainer(Long.parseLong(body.get(0)));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            }
        });
        this.addHandler("update maintainer", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 4)
                    throw new AppException("Different number of arguments to update maintainer");
                this.maintainerService.updateMaintainer(Long.parseLong(body.get(0)), body.get(1), body.get(2), body.get(3));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            }
        });
        this.addHandler("show filtered maintainers", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 2)
                    throw new AppException("Different number of arguments to show filtered maintainer");
                var maintainers = this.maintainerService.getFilteredMaintainers(body.get(0), body.get(1));
                List<String> strings = maintainers.get().map(NetworkingHelper::serialize).collect(Collectors.toList());
                return NetworkingHelper.success(strings);
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            } catch (InterruptedException | ExecutionException e) {
                return new Message(Message.MessageError, "Threading error");
            }
        });
        this.addHandler("show maintainers", request ->
        {
            try {
                var body = request.getBody();
                var maintainers = this.maintainerService.getAllMaintainers();
                List<String> strings = maintainers.get().map(NetworkingHelper::serialize).collect(Collectors.toList());
                return NetworkingHelper.success(strings);
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            } catch (InterruptedException | ExecutionException e) {
                return new Message(Message.MessageError, "Threading error");
            }
        });
    }

    private void addPackageHandlers()
    {
        this.addHandler("add package", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 4)
                    throw new AppException("Different number of arguments to create package");
                this.packageService.addPackage(body.get(0), body.get(1), body.get(2), body.get(3));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
        });
        this.addHandler("delete package", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 1)
                    throw new AppException("Different number of arguments to delete package");
                this.packageService.deletePackage(Long.parseLong(body.get(0)));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            }
        });
        this.addHandler("update package", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 5)
                    throw new AppException("Different number of arguments to update package");
                this.packageService.updatePackage(Long.parseLong(body.get(0)), body.get(1), body.get(2), body.get(3), body.get(4));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            }
        });
        this.addHandler("show filtered packages", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 2)
                    throw new AppException("Different number of arguments to show filtered package");
                var packages = this.packageService.getFilteredPackages(body.get(0), body.get(1));
                List<String> strings = packages.get().map(NetworkingHelper::serialize).collect(Collectors.toList());
                return NetworkingHelper.success(strings);
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            } catch (InterruptedException | ExecutionException e) {
                return new Message(Message.MessageError, "Threading error");
            }
        });
        this.addHandler("show packages", request ->
        {
            try {
                var body = request.getBody();
                var packages = this.packageService.getAllPackages();
                List<String> strings = packages.get().map(NetworkingHelper::serialize).collect(Collectors.toList());
                return NetworkingHelper.success(strings);
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            } catch (InterruptedException | ExecutionException e) {
                return new Message(Message.MessageError, "Threading error");
            }
        });
    }

    private void addPackageMaintainerHelpers()
    {
        this.addHandler("add package-maintainer", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 2)
                    throw new AppException("Different number of arguments to create package-maintainer");
                this.packageMaintainerService.addPackageMaintainer(Long.parseLong(body.get(0)), Long.parseLong(body.get(1)));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
        });
        this.addHandler("delete package-maintainer", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 1)
                    throw new AppException("Different number of arguments to delete package-maintainer");
                this.packageMaintainerService.deletePackageMaintainer(Long.parseLong(body.get(0)));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            }
        });
        this.addHandler("update package-maintainer", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 3)
                    throw new AppException("Different number of arguments to update package-maintainer");
                this.packageMaintainerService.updatePackageMaintainer(Long.parseLong(body.get(0)), Long.parseLong(body.get(1)), Long.parseLong(body.get(2)));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            }
        });
        this.addHandler("show filtered package-maintainers", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 2)
                    throw new AppException("Different number of arguments to show filtered package-maintainers");
                var packageMaintainers = this.packageMaintainerService.getFilteredPackageMaintainers(body.get(0), body.get(1));
                List<String> strings = packageMaintainers.get().map(NetworkingHelper::serialize).collect(Collectors.toList());
                return NetworkingHelper.success(strings);
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            } catch (InterruptedException | ExecutionException e) {
                return new Message(Message.MessageError, "Threading error");
            }
        });
        this.addHandler("show package-maintainers", request ->
        {
            try {
                var body = request.getBody();
                var packageMaintainers = this.packageMaintainerService.getAllPackageMaintainers();
                List<String> strings = packageMaintainers.get().map(NetworkingHelper::serialize).collect(Collectors.toList());
                return NetworkingHelper.success(strings);
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            } catch (InterruptedException | ExecutionException e) {
                return new Message(Message.MessageError, "Threading error");
            }
        });
    }

    private void addPackageVersionHandlers() {
        this.addHandler("add packageVersion", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 2)
                    throw new AppException("Different number of arguments to create packageVersion");
                this.packageVersionService.addPackageVersion(Long.parseLong(body.get(0)), body.get(1));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
        });
        this.addHandler("delete packageVersion", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 1)
                    throw new AppException("Different number of arguments to delete packageVersion");
                this.packageVersionService.deletePackageVersion(Long.parseLong(body.get(0)));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            }
        });
        this.addHandler("update packageVersion", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 3)
                    throw new AppException("Different number of arguments to update packageVersion");
                this.packageVersionService.updatePackageVersion(Long.parseLong(body.get(0)), Long.parseLong(body.get(1)), body.get(2));
                return new Message(Message.MessageSuccess, "");
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            }
        });
        this.addHandler("show filtered packageVersions", request ->
        {
            try {
                var body = request.getBody();
                if (body.size() != 2)
                    throw new AppException("Different number of arguments to show filtered package-maintainers");
                var packageVersions = this.packageVersionService.getFilteredPackageVersions(body.get(0), body.get(1));
                List<String> strings = packageVersions.get().map(NetworkingHelper::serialize).collect(Collectors.toList());
                return NetworkingHelper.success(strings);
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            } catch (InterruptedException | ExecutionException e) {
                return new Message(Message.MessageError, "Threading error");
            }
        });
        this.addHandler("show packageVersions", request ->
        {
            try {
                var body = request.getBody();
                var packageVersions = this.packageVersionService.getAllPackageVersions();
                List<String> strings = packageVersions.get().map(NetworkingHelper::serialize).collect(Collectors.toList());
                return NetworkingHelper.success(strings);
            }
            catch (AppException e)
            {
                return new Message(Message.MessageError, e.getMessage());
            }
            catch (NumberFormatException e)
            {
                return new Message(Message.MessageError, "There should be one argument of type long");
            } catch (InterruptedException | ExecutionException e) {
                return new Message(Message.MessageError, "Threading error");
            }
        });
    }

    private class ClientHandler implements Runnable {
        private Socket socket;

        public ClientHandler(Socket socket) {
            this.socket = socket;
        }

        @Override
        public void run() {
            try (var is = socket.getInputStream();
                 var os = socket.getOutputStream()) {

                //read the request (of type Message) from client
                Message request = Message.read(is);
                System.out.println("received request: " + request.getHeader());

                // compute response (of type Message)
                Message response = methodHandlers.get(request.getHeader()).apply(request);
                System.out.println("computed response: " + response.getHeader());

                //send response (of type Message) to client
                Message.write(response, os);
                System.out.println("response sent to client");

            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
