
set(trackerHeaders_hpp tracker/Camera.hpp
        tracker/ChessboardCameraTrackerKLT.hpp
        tracker/ChessboardCameraTracker.hpp
        tracker/utility.hpp
        tracker/ICameraTracker.hpp)

add_library( tracker STATIC utility.cpp ChessboardCameraTracker.cpp ChessboardCameraTrackerKLT.cpp Camera.cpp ${trackerHeaders_hpp})
target_link_libraries( tracker ${OpenCV_LIBS} )

install(TARGETS tracker LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
install(FILES ${trackerHeaders_hpp} DESTINATION include/)
