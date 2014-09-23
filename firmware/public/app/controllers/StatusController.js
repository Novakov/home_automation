'use strict';

angular.module('HomeAutomation.Controllers')
    .controller('StatusController', function ($scope, $http) {
        $scope.memory = {
            total: 0,
            allocated: 0,
        };

        $scope.disks = [];

        $http.get('/vm/status').success(function (data) {
            var oneKileByte = 1 * 1024;
            var oneMegaByte = 1 * 1024 * oneKileByte;
            var oneGigaByte = 1 * 1024 * oneMegaByte;

            $scope.memory.total = data.memory.total / oneMegaByte;
            $scope.memory.allocated = data.memory.allocated / oneMegaByte;
            $scope.memory.ratio = data.memory.allocated / data.memory.total;

            $scope.disks = $.map(data.disks, function (item) {
                return {
                    id: item.id,
                    size: item.size * oneKileByte / oneGigaByte,
                    used: item.size * oneKileByte * (item.used / 100) / oneGigaByte,
                    ratio: item.used/100
                }
            });
        });
    });