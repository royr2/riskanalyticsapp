// Custom JavaScript for ML Analytics Suite

// Initialize when document is ready
$(document).ready(function() {
  
  // Add smooth transitions
  $('body').addClass('loaded');
  
  // Progress tracking for both ML workflow and dashboard features
  var mlSteps = ['data_import', 'data_preprocessing', 'column_selection', 
                 'feature_selection', 'model_selection', 'model_training', 'model_diagnostics'];
  var dashboardSteps = ['overview_dashboard', 'charts_dashboard', 'kpi_dashboard', 'reports_dashboard'];
  
  function updateProgress(currentStep) {
    var currentIndex = mlSteps.indexOf(currentStep);
    if (currentIndex === -1) return; // Not an ML workflow step
    
    var progress = ((currentIndex + 1) / mlSteps.length) * 100;
    
    // Create or update progress bar for ML workflow
    if ($('#workflow-progress').length === 0) {
      $('<div id="workflow-progress" class="progress" style="margin: 10px;">' +
        '<div class="progress-bar" role="progressbar" style="width: 0%"></div>' +
        '<div class="progress-label">ML Workflow Progress</div>' +
        '</div>').prependTo('.content-wrapper');
    }
    
    $('#workflow-progress .progress-bar').css('width', progress + '%');
  }
  
  // Dashboard-specific enhancements
  function initializeDashboardFeatures() {
    // Auto-refresh dashboard data every 30 seconds
    setInterval(function() {
      if (typeof Shiny !== 'undefined') {
        var currentTab = $('.tab-pane.active').attr('id');
        if (currentTab && dashboardSteps.some(step => currentTab.includes(step))) {
          console.log('Auto-refreshing dashboard data...');
          Shiny.setInputValue('dashboard_auto_refresh', Math.random());
        }
      }
    }, 30000);
    
    // Enhanced chart interactions
    $(document).on('plotly_hover', '.plotly', function(event, data) {
      if (data.points && data.points.length > 0) {
        $(this).css('cursor', 'crosshair');
      }
    });
    
    $(document).on('plotly_unhover', '.plotly', function() {
      $(this).css('cursor', 'default');
    });
    
    // KPI dashboard enhancements
    $('.small-box').hover(
      function() { $(this).addClass('hover-effect'); },
      function() { $(this).removeClass('hover-effect'); }
    );
    
    // Chart export functionality
    window.exportChart = function(chartId, format) {
      format = format || 'png';
      var chart = document.getElementById(chartId);
      if (chart && typeof Plotly !== 'undefined') {
        Plotly.downloadImage(chart, {
          format: format,
          width: 1200,
          height: 800,
          filename: 'chart_export_' + new Date().getTime()
        });
        showNotification('Chart exported successfully!', 'success');
      }
    };
  }
  
  // Initialize dashboard features
  initializeDashboardFeatures();
    }
    
    $('#workflow-progress .progress-bar').css('width', progress + '%');
  }
  
  // Navigation enhancement
  $('.sidebar-menu a').on('click', function() {
    var tabName = $(this).parent().attr('data-tab');
    if (tabName) {
      updateProgress(tabName);
    }
  });
  
  // Loading indicators
  function showLoading(elementId) {
    $('#' + elementId).addClass('loading');
    $('#' + elementId).append('<div class="loading-overlay"><div class="loading-spinner"></div></div>');
  }
  
  function hideLoading(elementId) {
    $('#' + elementId).removeClass('loading');
    $('#' + elementId + ' .loading-overlay').remove();
  }
  
  // Auto-scroll to top when switching tabs
  $('.sidebar-menu a').on('click', function() {
    $('html, body').animate({scrollTop: 0}, 300);
  });
  
  // Enhanced tooltip functionality
  $('[data-toggle="tooltip"]').tooltip();
  
  // Auto-resize textareas
  $('textarea').each(function() {
    this.setAttribute('style', 'height:' + (this.scrollHeight) + 'px;overflow-y:hidden;');
  }).on('input', function() {
    this.style.height = 'auto';
    this.style.height = (this.scrollHeight) + 'px';
  });
  
  // Confirmation dialogs for important actions
  $('button[id*="train_model"], button[id*="run_diagnostics"]').on('click', function(e) {
    var buttonText = $(this).text();
    if (buttonText.includes('Train') || buttonText.includes('Diagnostic')) {
      var confirmed = confirm('This operation may take some time. Do you want to continue?');
      if (!confirmed) {
        e.preventDefault();
        e.stopPropagation();
        return false;
      }
    }
  });
  
  // Enhanced file upload feedback
  $('input[type="file"]').on('change', function() {
    var fileName = $(this).val().split('\\').pop();
    if (fileName) {
      $(this).next('.file-feedback').remove();
      $(this).after('<div class="file-feedback text-success">Selected: ' + fileName + '</div>');
    }
  });
  
  // Real-time validation feedback
  $('.form-control').on('blur', function() {
    if ($(this).val() === '' && $(this).prop('required')) {
      $(this).addClass('is-invalid');
    } else {
      $(this).removeClass('is-invalid');
    }
  });
  
  // Keyboard shortcuts
  $(document).keydown(function(e) {
    // Ctrl+Enter to trigger main action buttons
    if (e.ctrlKey && e.keyCode === 13) {
      var activeTab = $('.tab-pane.active');
      var primaryButton = activeTab.find('.btn-primary:visible').first();
      if (primaryButton.length) {
        primaryButton.click();
      }
    }
  });
  
  // Auto-save functionality (placeholder)
  function autoSave() {
    // This would save the current state
    console.log('Auto-saving current state...');
  }
  
  // Auto-save every 2 minutes
  setInterval(autoSave, 120000);
  
  // Add animation classes for smooth transitions
  $('.box').addClass('animated fadeIn');
  
  // Responsive table handling
  $('.table-responsive').on('shown.bs.collapse', function() {
    $(this).find('table').trigger('resize');
  });
  
});

// Global functions for Shiny integration
window.MLModelBuilder = {
  showNotification: function(message, type) {
    type = type || 'info';
    var alertClass = 'alert-' + type;
    var notification = $('<div class="alert ' + alertClass + ' alert-dismissible">' +
      '<button type="button" class="close" data-dismiss="alert">&times;</button>' +
      message + '</div>');
    
    $('#notifications').append(notification);
    
    // Auto-remove after 5 seconds
    setTimeout(function() {
      notification.fadeOut(function() {
        $(this).remove();
      });
    }, 5000);
  },
  
  updateWorkflowProgress: function(step) {
    var steps = ['data_import', 'data_preprocessing', 'column_selection', 
                 'feature_selection', 'model_selection', 'model_training', 'model_diagnostics'];
    var currentIndex = steps.indexOf(step);
    var progress = ((currentIndex + 1) / steps.length) * 100;
    
    $('#workflow-progress .progress-bar').css('width', progress + '%');
  }
};