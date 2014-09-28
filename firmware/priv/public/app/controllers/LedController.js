'use strict';

angular.module('HomeAutomation.Controllers')
    .controller('LedController', function ($scope, $http, $timeout) {
        $scope.led = 'unknown';
        $scope.label = function() {
            if ($scope.led == 'on') {
                return 'Turn off';
            } else {
                return 'Turn on';
            }
        }

        $http.get('/green')
            .success(function (data) {                
                $scope.led = data.status;
            });

        $scope.toggleLed = function () {
            if ($scope.led === 'on') {
                $http.post('/green/off').then(function () { $scope.led = 'off'; });
            } else {
                $http.post('/green/on').then(function () { $scope.led = 'on'; });
            }
        };
    });