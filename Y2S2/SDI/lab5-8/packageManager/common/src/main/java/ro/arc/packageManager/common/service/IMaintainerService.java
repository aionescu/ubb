package ro.arc.packageManager.common.service;

import ro.arc.packageManager.common.domain.Maintainer;
import ro.arc.packageManager.common.domain.validators.ValidatorException;

import java.util.concurrent.Future;
import java.util.stream.Stream;

public interface IMaintainerService {

    Future<Void> addMaintainer(String userName, String fullName, String email) throws ValidatorException;

    Future<Void> updateMaintainer(Long ID, String userName, String fullName, String email) throws ValidatorException;

    Future<Void> deleteMaintainer(Long ID);

    Future<Stream<Maintainer>> getFilteredMaintainers(String type, String input);

    Future<Stream<Maintainer>> getAllMaintainers();
}
