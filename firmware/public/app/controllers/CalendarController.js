'use strict';

angular.module('HomeAutomation.Controllers')
    .controller('CalendarController', function ($scope, $http, $modal) {
        $scope.eventSources = {
            url: '/events',
            eventDataTransform: function (e) {
                return {
                    id: e.series_id,
                    start: e.from,
                    end: e.to
                }
            }
        };

        $scope.addNewEvent = function (start, end) {
            var modalInstance = $modal.open({
                templateUrl: 'app/partials/new_event.html',
                controller: NewEventController,
                resolve: {
                    start_at: function () { return start; },
                    end_at: function () { return end; }
                }
            });

            modalInstance.result.then(function () {
               
            });
        };

        $scope.uiConfig = {
            calendar: {
                defaultView: 'agendaWeek',
                height: 750,
                firstDay: 1,
                allDaySlot: false,
                editable: true,
                selectable: true,
                selectHelper: true,
                columnFormat: {
                    week: 'd.MM dddd'
                },
                axisFormat: { 'agenda': 'H:mm' },
                timeFormat: 'H:mm',

                select: $scope.addNewEvent,
                startParam: 'from',
                endParam: 'to'
            }
        };
    });

var NewEventController = function ($scope, $modalInstance, start_at, end_at) {
    $scope.start_at = new Date();
    $scope.end_at = end_at;

    $scope.opened = false;

    $scope.ok = function() {
        $modalInstance.close();
    };
    $scope.cancel = function() {
        $modalInstance.dismiss('cancel');
    };

    $scope.open = function ($event) {        
        $event.preventDefault();
        $event.stopPropagation();
        
        $scope.opened = true;
        console.log('.');
    };

    $scope.dateOptions = {
        formatYear: 'yy',
        startingDay: 1,
        "show-weeks": false
    };

    $scope.datePopupOptions = {
        "show-button-bar":false
    };
};