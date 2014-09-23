'use strict';

angular.module('HomeAutomation.Controllers')
    .controller('CornerController', function ($scope, $interval, $http) {
        $scope.now = new Date();

        $interval(function () {
            $http.get('/now').success(function(data) {
                $scope.now = data.date;
            });
        }, 1000);
    });