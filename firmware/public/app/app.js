'use strict';

angular.module('HomeAutomation', ['HomeAutomation.Controllers', 'ngRoute'])
    .config(function ($routeProvider) {
        $routeProvider.when('/led', { templateUrl: 'app/partials/led.html', controller: 'LedController' })
            .otherwise({ redirectTo: '/led' });
    });