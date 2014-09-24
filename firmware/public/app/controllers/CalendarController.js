'use strict';

angular.module('HomeAutomation.Controllers')
    .controller('CalendarController', function($scope) {
        $scope.eventSources = {
            url: '/events'
        };

        $scope.uiConfig = {
            calendar: {
                defaultView: 'agendaWeek',
                height: 750,
                firstDay: 1,
                allDaySlot: false,
                editable:true,
                columnFormat: {
                    week: 'D.MM dddd'
                },
                axisFormat: { 'agenda': 'H:mm' },
                timeFormat: 'H:mm'
            }
        };
    });