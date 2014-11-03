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
            .when('/temperature/live', { templateUrl: 'app/partials/temperature.html', controller: 'TemperatureController' })
            .when('/temperature/history', { templateUrl: 'app/partials/temperature_history.html', controller: 'TemperatureHistoryController' })
            .otherwise({ redirectTo: '/led' });       
    });