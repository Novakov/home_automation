'use strict';

angular.module('HomeAutomation.Controllers')
    .controller('NavController', function ($scope, $location) {
        $scope.isActive = function (route) {
            return route === $location.path();
        };
    });