'use strict';

angular.module('HomeAutomation.Controllers')
    .controller('RootController', function () {

    });

angular.module('HomeAutomation', ['HomeAutomation.Controllers', 'ngRoute'])
    .config(function ($routeProvider, $provide) {
        $routeProvider
            .when('/led', { templateUrl: 'app/partials/led.html', controller: 'LedController' })
            .when('/status', { templateUrl: 'app/partials/status.html', controller: 'StatusController' })
            .when('/calendar', { templateUrl: 'app/partials/calendar.html', controller: 'CalendarController' })
            .otherwise({ redirectTo: '/led' });       
    });