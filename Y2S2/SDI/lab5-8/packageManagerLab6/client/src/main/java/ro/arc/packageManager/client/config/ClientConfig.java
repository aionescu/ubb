package ro.arc.packageManager.client.config;

import org.postgresql.shaded.com.ongres.scram.common.bouncycastle.pbkdf2.Pack;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.remoting.rmi.RmiProxyFactoryBean;
import ro.arc.packageManager.client.service.MaintainerServiceClientImplementation;
import ro.arc.packageManager.client.service.PackageMaintainerServiceClientImplementation;
import ro.arc.packageManager.client.service.PackageServiceClientImplementation;
import ro.arc.packageManager.client.service.PackageVersionServiceClientImplementation;
import ro.arc.packageManager.client.ui.ConsoleClient;
import ro.arc.packageManager.common.domain.PackageVersion;
import ro.arc.packageManager.common.service.MaintainerService;
import ro.arc.packageManager.common.service.PackageMaintainerService;
import ro.arc.packageManager.common.service.PackageService;
import ro.arc.packageManager.common.service.PackageVersionService;

@Configuration
public class ClientConfig {

    @Bean
    RmiProxyFactoryBean rmiMaintainerProxyFactoryBean() {
        RmiProxyFactoryBean rmiProxyFactoryBean = new RmiProxyFactoryBean();
        rmiProxyFactoryBean.setServiceUrl("rmi://localhost:1099/MaintainerService");
        rmiProxyFactoryBean.setServiceInterface(MaintainerService.class);
        return rmiProxyFactoryBean;
    }

    @Bean
    ConsoleClient console(){
        return new ConsoleClient(maintainerService(), packageService(), packageMaintainerService(), packageVersionService());
    }

    @Bean
    MaintainerService maintainerService() {
        return new MaintainerServiceClientImplementation();
    }

    @Bean
    RmiProxyFactoryBean rmiPackageProxyFactoryBean(){
        RmiProxyFactoryBean rmiProxyFactoryBean = new RmiProxyFactoryBean();
        rmiProxyFactoryBean.setServiceUrl("rmi://localhost:1099/PackageService");
        rmiProxyFactoryBean.setServiceInterface(PackageService.class);
        return rmiProxyFactoryBean;
    }

    @Bean
    PackageService packageService() {return new PackageServiceClientImplementation();}

    @Bean
    RmiProxyFactoryBean rmiPackageVersionProxyFactoryBean(){
        RmiProxyFactoryBean rmiProxyFactoryBean = new RmiProxyFactoryBean();
        rmiProxyFactoryBean.setServiceUrl("rmi://localhost:1099/PackageVersionService");
        rmiProxyFactoryBean.setServiceInterface(PackageVersionService.class);
        return rmiProxyFactoryBean;
    }

    @Bean
    PackageVersionService packageVersionService(){return new PackageVersionServiceClientImplementation();}

    @Bean
    RmiProxyFactoryBean rmiPackageMaintainerProxyFactoryBean(){
        RmiProxyFactoryBean rmiProxyFactoryBean = new RmiProxyFactoryBean();
        rmiProxyFactoryBean.setServiceUrl("rmi://localhost:1099/PackageMaintainerService");
        rmiProxyFactoryBean.setServiceInterface(PackageMaintainerService.class);
        return rmiProxyFactoryBean;
    }

    @Bean
    PackageMaintainerService packageMaintainerService() {
      return new PackageMaintainerServiceClientImplementation();
    }
}
