angular.module('HomeAutomation.Controllers')
    .controller('TemperatureController', function ($scope, $http, $interval) {
        $scope.currentTemperature = null;

        $scope.temperatures = [
            {
                key: "Temperature",
                values: []
            }
        ];

        $scope.xAxisTickFormatFunction = function () {
            return function (d) {
                return d3.time.format('%H:%M:%S')(new Date(d));
            }
        }

        $interval(function () {
            $http.get('/temperature')
                .then(function (xhr) {
                    $scope.currentTemperature = xhr.data.temperature;

                    var newValues = $scope.temperatures[0].values;

                    newValues.push([new Date().getTime(), xhr.data.temperature]);

                    if (newValues.length > 100) {
                        newValues.shift();
                    }

                    $scope.temperatures[0].values = newValues;

                });
        }, 1000);
    });