angular.module('HomeAutomation.Controllers')
    .controller('TemperatureController', function ($scope, $http, $interval) {
        $scope.currentTemperature = null;


        $interval(function () {
            $http.get('/temperature')
                .then(function (xhr) {
                    $scope.currentTemperature = xhr.data.temperature;
                });
        }, 1000);
    });